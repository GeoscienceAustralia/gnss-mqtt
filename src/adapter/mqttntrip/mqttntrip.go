// MQTT Relay Adapter for NTRIP
package main

import (
    "fmt"
    "io/ioutil"
    "time"
    "flag"
    "github.com/umeat/go-ntrip/ntrip"
    mqtt "github.com/eclipse/paho.mqtt.golang"
    "github.com/geoscienceaustralia/go-rtcm/rtcm3"
)

func main() {
    broker := flag.String("broker", "tcp://localhost:1883", "MQTT broker")
    topic := flag.String("topic", "ALIC00AUS/#", "MQTT topic prefix")
    mount:= flag.String("mount", "http://localhost:2101/ALIC7", "NTRIP caster mountpoint to stream from")
    username := flag.String("username", "", "NTRIP username")
    password := flag.String("password", "", "NTRIP password")
    flag.Parse()

    server, _ := ntrip.NewServer(*mount)
    server.SetBasicAuth(*username, *password)

    opts := mqtt.NewClientOptions()
    opts.AddBroker(*broker)
    opts.SetDefaultPublishHandler(func(client mqtt.Client, msg mqtt.Message) {
        server.Write(rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize())
    })

    client := mqtt.NewClient(opts)
    if token := client.Connect(); token.Wait() && token.Error() != nil {
        panic(token.Error())
    }

    if token := client.Subscribe(*topic, 1, nil); token.Wait() && token.Error() != nil {
        panic(token.Error())
    }

    for ; ; time.Sleep(time.Second) {
        resp, err := server.Connect()
        if err != nil || resp.StatusCode != 200 {
            fmt.Println(resp, err)
            continue
        }
        fmt.Println("client connected")
        ioutil.ReadAll(resp.Body)
        fmt.Println("client connection closed")
    }
}
