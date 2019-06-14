// Implementation of a "NTRIP" to MQTT proxy
package main

import (
	"fmt"
	"time"
	"context"
	"net/http"
	"github.com/gorilla/mux"
	"github.com/google/uuid"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	log "github.com/sirupsen/logrus"
)

// Caster contains global configuration for handling connections and constructing sourcetable
type Caster struct {
	Port       string
	Hostname   string // TODO: Lookup?
	Identifier string
	Operator   string
	//	NMEA       bool
	Country string
	//	Latitude   float64
	//	Longitude  float64
	//	Fallback   string
	//	FallbackIP string
	//	Misc       string
	Mounts map[string]*Mount
}

// String representation of Caster in NTRIP Sourcetable entry format
func (caster *Caster) String() string {
	return fmt.Sprintf("CAS;%s;%s;%s;%s;0;%s;0;0;;",
		caster.Hostname, caster.Port, caster.Identifier, caster.Operator, caster.Country)
}

// GetSourcetable serves caster and mount information in NTRIP Sourcetable format
func (caster *Caster) GetSourcetable(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "%s\r\n", caster)
	for _, mount := range caster.Mounts {
		if mount.LastMessage.After(time.Now().Add(-time.Second * 3)) {
			fmt.Fprintf(w, "%s\r\n", mount)
		}
	}
	w.(http.Flusher).Flush()
}

// GetMount handles GET requests for Mounts, establishing an MQTT client and
// subscription for each request and streaming the data back to the client
func (caster *Caster) GetMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Check if mountpoint exists
	mount, exists := caster.Mounts[r.URL.Path[1:]]
	if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second*3)) {
		logger.Info("no existing mountpoint")
		w.WriteHeader(http.StatusNotFound)
		return
	}

	// Create MQTT client connection on behalf of HTTP user
	subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := subClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return // TODO: log error
	}
	defer subClient.Disconnect(100)

	// Subscribe to MQTT topic and forward messages to channel
	data := make(chan []byte)
	token := subClient.Subscribe(mount.Name+"/RTCM3/#", 1, func(client mqtt.Client, msg mqtt.Message) {
		data <- rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize() // Need an encapsulation method which takes []byte
	})
	if token.Wait() && token.Error() != nil {
		logger.Error("MQTT subscription failed - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	logger.Info("client connected")
	w.(http.Flusher).Flush() // Return 200

	for {
		select {
		// Read data from MQTT channel and write to HTTP connection
		case d := <-data:
			fmt.Fprintf(w, "%s\r\n", d)
			w.(http.Flusher).Flush()
		case <-r.Context().Done():
			logger.Info("client disconnected")
			return
		case <-time.After(time.Second * 3):
			logger.Info("timeout reading from MQTT subscription channel")
			return
		}
	}
}

// PostMount handles POST requests to a Mount, parsing the stream as RTCM and
// forwarding to MQTT broker
// TODO: Currently not checking if the mount is in caster.Mounts or if it's
// currently up, so can have multiple publishers on the same topic. I don't
// know if there's a reasonable way to avoid this
func (caster *Caster) PostMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Create MQTT client connection on behalf of HTTP user
	pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := pubClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	defer pubClient.Disconnect(100)

	logger.Info("client connected")
	w.Header().Set("Connection", "close")
	w.(http.Flusher).Flush()
	// Without this sleep it looks like we're reading from the body before the POSTer has sent any data, which returns an error
	// Need to find a better way of waiting until we have received data / timing out
	time.Sleep(1 * time.Second)

	// Scan POSTed data for RTCM messages and publish with MQTT client
	scanner := rtcm3.NewScanner(r.Body)
	rtcmFrame, err := scanner.NextFrame()
	for ; err == nil; rtcmFrame, err = scanner.NextFrame() {
		pubClient.Publish(fmt.Sprintf("%s/RTCM3/%d", r.URL.Path[1:], rtcmFrame.MessageNumber()), 1, false, rtcmFrame.Payload)
	}
	log.Error("stream ended - " + err.Error())
}

// Mount represents a NTRIP mountpoint which proxies through to an MQTT topic
// TODO: Clients could subscribe to a Mount so each client doesn't need a MQTT connection - less load on MQTT broker, but maybe not necessary
// TODO: Handle concurrent writes to Mount using a lock, concurrent writes to LastMessage could break
type Mount struct {
	Name          string
	Identifier    string // meta
	Format        string // stream - minor version doesn't really matter since RTCM3 is strictly additive
	FormatDetails string // stream
	Carrier       string // stream
	NavSystem     string // stream
	Network       string // meta
	CountryCode   string // meta
	Latitude      string // meta / stream
	Longitude     string // meta / stream
	//	NMEA           bool
	//	Solution       bool
	Generator string // stream
	//	Compression    string
	//	Authentication string
	//	Fee            bool
	//	Bitrate        int
	Misc        string
	LastMessage time.Time
}

// String representation of Mount in NTRIP Sourcetable entry format
func (mount *Mount) String() string {
	return fmt.Sprintf("STR;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;0;0;%s;none;N;N;0;%s",
		mount.Name, mount.Identifier, mount.Format, mount.FormatDetails, mount.Carrier,
		mount.NavSystem, mount.Network, mount.CountryCode, mount.Latitude, mount.Longitude,
		mount.Generator, mount.Misc)
}

var ( // TODO: Define from config file - would like to find config manager which is capable of invoking a goroutine for each element of list, as well as for new elements added to the list (watcher functions for mounts)
	broker = "tcp://localhost:1883"
	caster = &Caster{"2101", "go-ntrip.geops.team", "NTRIP to MQTT Proxy", "GA", "AUS", map[string]*Mount{
		"SYMY00AUS": &Mount{Name: "SYMY00AUS", LastMessage: time.Unix(0, 0), Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"ALIC00AUS": &Mount{Name: "ALIC00AUS", LastMessage: time.Unix(0, 0), Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"YAR200AUS": &Mount{Name: "YAR200AUS", LastMessage: time.Unix(0, 0), Identifier: "Yarragadee (WA)", Format: "RTCM 3.3"},
		"PARK00AUS": &Mount{Name: "PARK00AUS", LastMessage: time.Unix(0, 0), Identifier: "Parkes (NSW)", Format: "RTCM 3.3"},
		"ALBU00AUS": &Mount{Name: "ALBU00AUS", LastMessage: time.Unix(0, 0), Identifier: "Albury (NSW)", Format: "RTCM 3.3"},
		"DAV100AUS": &Mount{Name: "DAV100AUS", LastMessage: time.Unix(0, 0), Identifier: "Davis Station", Format: "RTCM 3.3"},
		"TEST00AUS": &Mount{Name: "TEST00AUS", LastMessage: time.Unix(0, 0), Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST04AUS": &Mount{Name: "TEST04AUS", LastMessage: time.Unix(0, 0), Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST05AUS": &Mount{Name: "TEST05AUS", LastMessage: time.Unix(0, 0), Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
	}}
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})

	// Watcher subscriptions update last received message on Mount objects so 404s and timeouts can be implemented
	// TODO: Create these subscriptions on initialization of / changes to config
	mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer mqttClient.Disconnect(100)

	// There should be no harm in just resubscribing for all mounts on any change to config
	for _, mount := range caster.Mounts {
		go func(mount *Mount) { // This doesn't need to be a go routine, but mount does need to be copied so the anonymous function passed to Subscribe isn't a closure referencing the for loop's mount variable
			token := mqttClient.Subscribe(mount.Name+"/RTCM3/#", 1, func(client mqtt.Client, msg mqtt.Message) {
				mount.LastMessage = time.Now()
			})
			if token.Wait() && token.Error() != nil {
				panic(token.Error()) //TODO: handle properly
			}
		}(mount)
	}

	httpMux := mux.NewRouter()
	httpMux.HandleFunc("/", caster.GetSourcetable).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", caster.GetMount).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", caster.PostMount).Methods("POST")

	// This is probably fancier than it needs to be, could really just have a GetLogger function so we don't have to cast when pulling out the logger
	// Perhaps a mix of both makes sense, where the UUID is added to context, but you generate a logger from the Request (which includes the context) when you need it
	// One benefit of defining the logger in the Context is that it can be appended to
	httpMux.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			requestId := uuid.New().String()
			ctx := context.WithValue(r.Context(), "UUID", requestId)
			ctx = context.WithValue(ctx, "logger", log.WithFields(log.Fields{
				"request_id": requestId,
				"path":       r.URL.Path,
				"method":     r.Method,
				"source_ip":  r.RemoteAddr,
			}))
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	})

	log.Fatal(http.ListenAndServe(":"+caster.Port, httpMux))
}
