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

	opts := mqtt.NewClientOptions().
		AddBroker(*broker).
		SetOnConnectHandler(func(client mqtt.Client) {
			if token := client.Subscribe(*topic, 1, nil); token.Wait() && token.Error() != nil {
				panic(token.Error())
			}
		})


	opts.SetDefaultPublishHandler(func(client mqtt.Client, mqttmsg mqtt.Message) {
		msg := rtcm3.DeserializeMessage(mqttmsg.Payload()) // DeserializeMessage should return error
		if obs, ok := msg.(rtcm3.Observable); ok {
			fmt.Println(time.Now().Format("2006-01-02T15:04:05.999999"), mqttmsg.Topic(), obs.Number(), len(mqttmsg.Payload()), time.Now().UTC().Sub(obs.Time()))
		} else {
			fmt.Println(time.Now().Format("2006-01-02T15:04:05.999999"), mqttmsg.Topic(), msg.Number(), len(mqttmsg.Payload()))
		}
	})

	client := mqtt.NewClient(opts)
	if token := client.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	select {}
}
