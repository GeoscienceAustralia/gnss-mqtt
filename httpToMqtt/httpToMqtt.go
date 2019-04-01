// Extremely naive implementation of a "NTRIP" to MQTT proxy
package main

import (
	"time"
	"log"
	"fmt"
	"net/http"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	mqtt "github.com/eclipse/paho.mqtt.golang"
)

// TODO: Clients subscribe to Mount so each client doesn't need a MQTT connection
type Mount struct {
	Name string
	// Used to decide whether or not an MQTT stream is still "active"
	LastMessage time.Time
	SourceInformation SourceInformation
}

// These could more simply be attributes of Mount
type SourceInformation struct {
	Identifier string
	Format string
	FormatDetails string
	Carrier string
	NavSystem string
	Network string
	CountryCode string
	Latitude string
	Longitude string
	NMEA string
	Solution string
	Generator string
	Compression string
	Authentication string
	Fee string
	Bitrate string
	Misc string
}

// NTRIP Sourcetable entry format
func (mount *Mount) String() string {
	return fmt.Sprintf("STR;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s",
		mount.Name, mount.SourceInformation.Identifier, mount.SourceInformation.Format,
		mount.SourceInformation.FormatDetails, mount.SourceInformation.Carrier,
		mount.SourceInformation.NavSystem, mount.SourceInformation.Network,
		mount.SourceInformation.CountryCode, mount.SourceInformation.Latitude,
		mount.SourceInformation.Longitude, mount.SourceInformation.NMEA,
		mount.SourceInformation.Solution, mount.SourceInformation.Generator,
		mount.SourceInformation.Compression, mount.SourceInformation.Authentication,
		mount.SourceInformation.Fee, mount.SourceInformation.Bitrate, mount.SourceInformation.Misc)
}

var ( // TODO: Define mounts from config
	broker = "tcp://35.189.48.59:1883"
	mounts = map[string]*Mount{
		"SYMY": &Mount{"SYMY", time.Unix(0, 0), SourceInformation{Identifier: "Canberra (ACT)", Format: "RTCM3.2"}},
		"TEST1": &Mount{"TEST1", time.Unix(0, 0), SourceInformation{Identifier: "Canberra (ACT)", Format: "RTCM3.2"}},
		"TEST2": &Mount{"TEST2", time.Unix(0, 0), SourceInformation{Identifier: "Canberra (ACT)", Format: "RTCM3.2"}},
		"TEST3": &Mount{"TEST3", time.Unix(0, 0), SourceInformation{Identifier: "Canberra (ACT)", Format: "RTCM3.2"}},
	}
)

func main() {
	mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer mqttClient.Disconnect(0)

	for _, mount := range mounts { // Create these subscriptions on initialization of / changes to config
		go func(mount *Mount) { // This doesn't need to be a go routine, but mount does need to be copied so the anonymous function passed to Subscribe isn't a closure
			token := mqttClient.Subscribe(mount.Name + "/#", 1, func(client mqtt.Client, msg mqtt.Message) {
				mount.LastMessage = time.Now()
			})
			if token.Wait() && token.Error() != nil {
				panic(token.Error())
			}
		}(mount)
	}

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		defer r.Body.Close()
		if r.URL.Path == "/" { // Return sourcetable
			for _, mount := range mounts {
				if mount.LastMessage.After(time.Now().Add(-time.Second * 3)) {
					fmt.Fprintf(w, "%s\r\n", mount)
				}
			}
			w.(http.Flusher).Flush()
			return
		}

		switch r.Method {
		case http.MethodGet: // Create MQTT connection on behalf of the client and subscribe to Topic of Mount.Name 
			mount, exists := mounts[r.URL.Path[1:]]
			if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second * 3)) {
				w.WriteHeader(http.StatusNotFound)
				return
			}

			w.(http.Flusher).Flush() // Return 200

			subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
			if token := subClient.Connect(); token.Wait() && token.Error() != nil {
				panic(token.Error())
			}

			data := make(chan []byte)
			token := subClient.Subscribe(mount.Name + "/#", 1, func(client mqtt.Client, msg mqtt.Message) {
				data <- rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize()
			})
			if token.Wait() && token.Error() != nil {
				panic(token.Error())
			}

			for { // TODO: Implement timeout
				fmt.Fprintf(w, "%s\r\n", <-data)
				w.(http.Flusher).Flush()
			}

		case http.MethodPost: // Could currently have multiple sources POSTing to a topic
			pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
			if token := pubClient.Connect(); token.Wait() && token.Error() != nil {
				panic(token.Error())
			}

			w.Header().Set("Connection", "close")
			w.(http.Flusher).Flush()
			time.Sleep(1 * time.Second) // Why? Without this it looks like we're reading from the body before the POSTer has sent any data

			scanner := rtcm3.NewScanner(r.Body)
			msg, err := scanner.Next()
			for ; err == nil; msg, err = scanner.Next() {
				pubClient.Publish(fmt.Sprintf("%s/%d", r.URL.Path[1:], msg.Number()), 1, false, msg.Serialize())
				//fmt.Fprintf(w, "\r\n")
				//w.(http.Flusher).Flush()
			}
			fmt.Println(err)
		}
	})

	log.Fatal(http.ListenAndServe(":2101", nil))
}
