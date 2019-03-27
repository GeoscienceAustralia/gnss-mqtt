// Extremely naive implementation of a "NTRIP" to MQTT proxy
package main

import (
	"encoding/json"
	"strings"
	"time"
	"log"
	"fmt"
	"net/http"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	mqtt "github.com/eclipse/paho.mqtt.golang"
)

var (
	mounts = map[string]time.Time{}
)

func main() {
	go Watcher()

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/" {
			j, _ := json.Marshal(mounts)
			fmt.Fprintf(w, string(j))
			w.(http.Flusher).Flush()
			return
		}

		// Need separate clients otherwise multiple the latest subscription to a topic will be the only one which receives messages for that topic
		opts := mqtt.NewClientOptions()
		opts.AddBroker("tcp://35.189.48.59:1883")
		mqttClient := mqtt.NewClient(opts)
		if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
			panic(token.Error())
		}

		defer r.Body.Close()
		switch r.Method {
		case http.MethodGet:
			if active, exists := mounts[r.URL.Path[1:]]; !exists || active.Before(time.Now().Add(-time.Second * 3)) {
				w.WriteHeader(http.StatusNotFound)
				return
			}
			channel := make(chan []byte, 10)
			token := mqttClient.Subscribe(r.URL.Path[1:] + "/#", 1, func(client mqtt.Client, msg mqtt.Message) {
				channel<-rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize()
			})
			if token.Wait() && token.Error() != nil {
				panic(token.Error())
			}
			w.(http.Flusher).Flush()

			for {
				data := <-channel
				fmt.Fprintf(w, "%s\r\n", data)
				w.(http.Flusher).Flush()
			}

		case http.MethodPost:
			w.Header().Set("Connection", "close")
			w.(http.Flusher).Flush()
			time.Sleep(1 * time.Second) // Why? Without this it looks like we're reading from the body before the POSTer has sent any data
			scanner := rtcm3.NewScanner(r.Body)
			msg, err := scanner.Next()
			for ; err == nil; msg, err = scanner.Next() {
				mqttClient.Publish(fmt.Sprintf("%s/%d", r.URL.Path[1:], msg.Number()), 1, false, msg.Serialize())
			}
			fmt.Println(err)
		}
	})

	log.Fatal(http.ListenAndServe(":2101", nil))
}

func Watcher() {
	opts := mqtt.NewClientOptions()
	opts.AddBroker("tcp://35.189.48.59:1883")
	mqttClient := mqtt.NewClient(opts)
	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	token := mqttClient.Subscribe("#", 1, func(client mqtt.Client, msg mqtt.Message) {
		mounts[strings.Split(msg.Topic(), "/")[0]] = time.Now()
	})
	if token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
}
