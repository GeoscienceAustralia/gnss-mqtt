// Implementation of a "NTRIP" to MQTT proxy
package main

import (
	"fmt"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	"github.com/gorilla/mux"
	"log"
	"net/http"
	"time"
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
	mount, exists := caster.Mounts[r.URL.Path[1:]]
	if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second*3)) {
		w.WriteHeader(http.StatusNotFound)
		return
	}

	w.(http.Flusher).Flush() // Return 200

	// TODO: Write NewMQTTClient function
	subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := subClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer subClient.Disconnect(0)

	data := make(chan []byte)
	token := subClient.Subscribe(mount.Name+"/#", 1, func(client mqtt.Client, msg mqtt.Message) {
		data <- rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize()
	})
	if token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	for {
		select {
		case d := <-data:
			fmt.Fprintf(w, "%s\r\n", d)
			w.(http.Flusher).Flush()
		case <-time.After(time.Second * 3):
			return
		}
	}
}

// PostMount handles POST requests to a Mount, parsing the stream as RTCM and
// forwarding to MQTT broker
func (caster *Caster) PostMount(w http.ResponseWriter, r *http.Request) {
	pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := pubClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer pubClient.Disconnect(0)

	w.Header().Set("Connection", "close")
	w.(http.Flusher).Flush()
	time.Sleep(1 * time.Second) // Why? Without this it looks like we're reading from the body before the POSTer has sent any data

	// TODO: Probably only need to parse Frame, and get message number
	scanner := rtcm3.NewScanner(r.Body)
	msg, err := scanner.Next()
	for ; err == nil; msg, err = scanner.Next() {
		pubClient.Publish(fmt.Sprintf("%s/%d", r.URL.Path[1:], msg.Number()), 1, false, msg.Serialize())
	}
	fmt.Println(err)
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
	// Watcher subscriptions update last received message on Mount objects so 404s and timeouts can be implemented
	// TODO: Create these subscriptions on initialization of / changes to config
	mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer mqttClient.Disconnect(0)

	// There should be no harm in just resubscribing for all mounts on any change to config
	for _, mount := range caster.Mounts {
		go func(mount *Mount) { // This doesn't need to be a go routine, but mount does need to be copied so the anonymous function passed to Subscribe isn't a closure referencing the for loop's mount variable
			token := mqttClient.Subscribe(mount.Name+"/#", 1, func(client mqtt.Client, msg mqtt.Message) {
				mount.LastMessage = time.Now()
			})
			if token.Wait() && token.Error() != nil {
				panic(token.Error())
			}
		}(mount)
	}

	httpMux := mux.NewRouter()
	httpMux.HandleFunc("/", caster.GetSourcetable).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", caster.GetMount).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", caster.PostMount).Methods("POST")

	log.Fatal(http.ListenAndServe(":"+caster.Port, httpMux))
}
