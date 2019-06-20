package main

import (
	"fmt"
	"time"
	"flag"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
    mqtt "github.com/eclipse/paho.mqtt.golang"
)

func main() {
	broker := flag.String("broker", "tcp://localhost:1883", "")
	topic := flag.String("topic", "#", "")
	flag.Parse()

	opts := mqtt.NewClientOptions().AddBroker(*broker)
    opts.SetDefaultPublishHandler(func(client mqtt.Client, mqttmsg mqtt.Message) {
		msg := rtcm3.DeserializeMessage(mqttmsg.Payload())
		switch int(msg.Number()) {
		case 1071, 1072, 1073, 1074, 1075, 1076, 1077,
			 1081, 1082, 1083, 1084, 1085, 1086, 1087,
			 1091, 1092, 1093, 1094, 1095, 1096, 1097,
			 1101, 1102, 1103, 1104, 1105, 1106, 1107,
			 1111, 1112, 1113, 1114, 1115, 1116, 1117,
			 1121, 1122, 1123, 1124, 1125, 1126, 1127,
			 1001, 1002, 1003, 1004, 1009, 1010, 1011, 1012:
			fmt.Println(msg.Number(), time.Now().UTC().Sub(msg.(rtcm3.Observable).Time()))

		default:
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
