package main

import (
	"flag"
	"fmt"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	"time"
)

func main() {
	broker := flag.String("broker", "tcp://localhost:1883", "")
	topic := flag.String("topic", "#", "")
	flag.Parse()

	opts := mqtt.NewClientOptions().AddBroker(*broker)
	opts.SetDefaultPublishHandler(func(client mqtt.Client, mqttmsg mqtt.Message) {
		msg := rtcm3.DeserializeMessage(mqttmsg.Payload())
		if obs, ok := msg.(rtcm3.Observable); ok {
			fmt.Println(msg.Number(), time.Now().UTC().Sub(obs.Time()))
		} else {
			fmt.Println(msg.Number())
		}
	})

	client := mqtt.NewClient(opts)
	if token := client.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	if token := client.Subscribe(*topic, 1, nil); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	select {}
}
