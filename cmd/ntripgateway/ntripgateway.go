package main

import (
	log "github.com/sirupsen/logrus"
	"github.com/geoscienceaustralia/gnss-mqtt/pkg/ntrip"
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})

	// TODO: Define from config file
	gateway, err := ntrip.NewGateway("2101", "streams.geops.team", "NTRIP Gateway for MQTT", "GA", "AUS", "tcp://localhost:1883")
	if err != nil {
		log.Fatal(err)
	}

	//TODO: Find config manager which will invoke AddMount on changes to Mounts in config file
	gateway.AddMount(&ntrip.Mount{Name: "TEST00AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntrip.Mount{Name: "TEST01AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntrip.Mount{Name: "TEST02AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntrip.Mount{Name: "TEST03AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntrip.Mount{Name: "TEST04AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntrip.Mount{Name: "TEST05AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})

	log.Fatal(gateway.Serve())
}
