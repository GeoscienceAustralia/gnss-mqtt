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
	SourceTableEntry string
	LastMessage time.Time
}

var ( // config
	mounts = map[string]*Mount{
		"SYMY": &Mount{"SYMY", "Canberra (ACT);RTCM3.2;;;;;;;;;", time.Unix(0, 0)},
		"TEST2": &Mount{"TEST2", "Sydney (NSW);RTCM3.2;;;;;;;;;", time.Unix(0, 0)},
	}
)

func main() {
	mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker("tcp://35.189.48.59:1883"))
	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer mqttClient.Disconnect()

	for _, mount := range mounts { // Construct these from new mounts added to config
		go func(mount *Mount) {
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
					fmt.Fprintf(w, "%s;%s\r\n", mount.Name, mount.SourceTableEntry)
				}
			}
			w.(http.Flusher).Flush()
			return
		}

		switch r.Method {
		case http.MethodGet:
			mount, exists := mounts[r.URL.Path[1:]]
			if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second * 3)) {
				w.WriteHeader(http.StatusNotFound)
				return
			}

			w.(http.Flusher).Flush() // Return 200

			subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker("tcp://35.189.48.59:1883"))
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

			for {
				fmt.Fprintf(w, "%s\r\n", <-data)
				w.(http.Flusher).Flush()
			}

		case http.MethodPost:
			pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker("tcp://35.189.48.59:1883"))
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
