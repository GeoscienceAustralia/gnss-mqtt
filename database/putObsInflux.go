// Naively stores the serialized binary representation of MSM messages in InfluxDB
package main

import (
	"time"
	"strings"
	"strconv"
	"encoding/base64"
    "github.com/geoscienceaustralia/go-rtcm/rtcm3"
	influx "github.com/influxdata/influxdb1-client/v2"
    mqtt "github.com/eclipse/paho.mqtt.golang"
)

func main() {
	influxClient, _ := influx.NewHTTPClient(influx.HTTPConfig{Addr: "http://localhost:8086"})
	defer influxClient.Close()

    mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker("tcp://localhost:1883"))
    if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
        panic(token.Error())
    }

    //mqttClient.SubscribeMultiple(messages, func(client mqtt.Client, msg mqtt.Message) {
    mqttClient.Subscribe("#", 1, func(client mqtt.Client, msg mqtt.Message) {
        rtcmMsg := rtcm3.DeserializeMessage(msg.Payload())
		pt, _ := influx.NewPoint(
			"message",
			map[string]string{
				"stationId": strings.Split(msg.Topic(), "/")[0],
				"messageNumber": strconv.Itoa(int(rtcmMsg.Number())),
			},
			map[string]interface{}{
				"data": base64.StdEncoding.EncodeToString(rtcmMsg.Serialize()),
			},
			time.Now(),
		)

		bp, _ := influx.NewBatchPoints(influx.BatchPointsConfig{
			Database:  "rtcm",
		})

		bp.AddPoint(pt)
		influxClient.Write(bp)
    })

    select {}
}
