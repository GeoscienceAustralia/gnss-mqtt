package main

import (
    "time"
    "fmt"
    "os"
    "flag"
    MQTT "github.com/eclipse/paho.mqtt.golang"
    "github.com/geoscienceaustralia/go-rtcm/rtcm3"
)

func main() {
    topic := flag.String("topic", "#", "MQTT Topic name, default all (#)")
    qos := flag.Int("qos", 0, "The Quality of Service 0,1,2 (default 0)")
    broker := flag.String("broker", "tcp://localhost:1883", "The broker URI. ex: tcp://10.10.1.1:1883")
    flag.Parse()

    opts := MQTT.NewClientOptions()
    opts.AddBroker(*broker)
    //opts.SetClientID("Latency")

    opts.SetDefaultPublishHandler(func(client MQTT.Client, mqttMsg MQTT.Message) {
        rtcmMsg := rtcm3.DeserializeMessage(mqttMsg.Payload())
        switch int(rtcmMsg.Number()) {
        case 1071, 1072, 1073, 1074, 1075, 1076, 1077,
             1081, 1082, 1083, 1084, 1085, 1086, 1087,
             1091, 1092, 1093, 1094, 1095, 1096, 1097,
             1111, 1112, 1113, 1114, 1115, 1116, 1117,
             1121, 1122, 1123, 1124, 1125, 1126, 1127,
             1001, 1002, 1003, 1004, 1009, 1010, 1011, 1012:
            fmt.Println(mqttMsg.Topic(), rtcmMsg.Number(), time.Now().UTC().Sub(rtcmMsg.(rtcm3.Observable).Time()))
        default:
            fmt.Println(mqttMsg.Topic(), rtcmMsg.Number())
        }
    })

    client := MQTT.NewClient(opts)
    if token := client.Connect(); token.Wait() && token.Error() != nil {
        panic(token.Error())
    }

    if token := client.Subscribe(*topic, byte(*qos), nil); token.Wait() && token.Error() != nil {
        fmt.Println(token.Error())
        os.Exit(1)
    }

    select {}
}
