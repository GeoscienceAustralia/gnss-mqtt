package ntrip

import (
	"fmt"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	log "github.com/sirupsen/logrus"
	"net/http"
	"time"
	"context"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
)

// Gateway contains global configuration for handling connections and constructing sourcetable
type Gateway struct { // Gateway might be a misnomer since this isn't strictly a broadcast server
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
	Broker string
}

// String representation of Gateway in NTRIP Sourcetable entry format
func (gateway *Gateway) String() string {
	return fmt.Sprintf("CAS;%s;%s;%s;%s;0;%s;0;0;;",
		gateway.Hostname, gateway.Port, gateway.Identifier, gateway.Operator, gateway.Country)
}

func (gateway *Gateway) Serve() error {
	{ // This could probably all be defined in NewMount
		// Watcher subscriptions update LastReceived attribute on Mount objects so 404s and timeouts can be implemented
		// TODO: Create these subscriptions on initialization of / changes to config
		mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(gateway.Broker))
		if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
			log.Fatal("failed to create MQTT client - " + token.Error().Error())
		}
		defer mqttClient.Disconnect(100)

		// There should be no harm in just resubscribing for all mounts on any change to config
		for _, mount := range gateway.Mounts {
			go func(mount *Mount) { // This doesn't need to be a function, but mount does need to be copied so the anonymous function passed to Subscribe isn't a closure referencing the for loop's mount variable
				token := mqttClient.Subscribe(mount.Name + "/#", 1, func(_ mqtt.Client, msg mqtt.Message) {
					mount.LastMessage = time.Now()
				})
				if token.Wait() && token.Error() != nil {
					log.Fatal("MQTT subscription failed - " + token.Error().Error()) // Should this be fatal?
				}
			}(mount)
		}
	}

	httpMux := mux.NewRouter()
	httpMux.HandleFunc("/", gateway.GetSourcetable).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", gateway.GetMount).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", gateway.PostMount).Methods("POST")

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

	log.Info("server starting")
	return http.ListenAndServe(":" + gateway.Port, httpMux)
}

// GetSourcetable serves gateway and mount information in NTRIP Sourcetable format
func (gateway *Gateway) GetSourcetable(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "%s\r\n", gateway)
	for _, mount := range gateway.Mounts {
		//TODO: Make timeout configurable and make the following if statement a method of Mount
		if mount.LastMessage.After(time.Now().Add(-time.Second * 3)) {
			fmt.Fprintf(w, "%s\r\n", mount)
		}
	}
	w.(http.Flusher).Flush()
}

// GetMount handles GET requests for Mounts, establishing an MQTT client and
// subscription for each request and streaming the data back to the client
func (gateway *Gateway) GetMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Check if mountpoint exists
	mount, exists := gateway.Mounts[r.URL.Path[1:]]
	if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second * 3)) {
		logger.Info("no existing mountpoint")
		w.WriteHeader(http.StatusNotFound)
		return
	}

	// Create MQTT client connection on behalf of HTTP user
	subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(gateway.Broker))
	if token := subClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return // TODO: log error
	}
	defer subClient.Disconnect(100)

	// Subscribe to MQTT topic and forward messages to channel
	data := make(chan []byte)
	token := subClient.Subscribe(mount.Name+"/#", 1, func(client mqtt.Client, msg mqtt.Message) {
		//TODO: Add an encapsulation method which takes []byte
		data <- rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize()
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
// TODO: Currently not checking if the mount is in gateway.Mounts or if it's
// currently up, so can have multiple publishers on the same topic. I don't
// know if there's a reasonable way to avoid this besides consulting LastMessage
func (gateway *Gateway) PostMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Create MQTT client connection on behalf of HTTP user
	pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(gateway.Broker))
	if token := pubClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	defer pubClient.Disconnect(100)

	w.Header().Set("Connection", "close")
	w.(http.Flusher).Flush()
	logger.Info("client connected")

	// Scan POSTed data for RTCM messages and publish with MQTT client
	scanner := rtcm3.NewScanner(r.Body)
	rtcmFrame, err := scanner.NextFrame()
	for ; err == nil; rtcmFrame, err = scanner.NextFrame() {
		pubClient.Publish(fmt.Sprintf("%s/%d", r.URL.Path[1:], rtcmFrame.MessageNumber()), 1, false, rtcmFrame.Payload)
	}
	log.Error("stream ended - " + err.Error())
}
