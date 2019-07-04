package main

import (
	log "github.com/sirupsen/logrus"
	"github.com/geoscienceaustralia/gnss-mqtt/pkg/ntripmqtt"
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})

	// TODO: Define from config file
	gateway, err := ntripmqtt.NewGateway("2101", "tcp://vernemq:1883")
	if err != nil {
		log.Fatal(err)
	}

	gateway.Hostname = "streams.geops.team"
	gateway.Operator = "Geoscience Australia"

	//TODO: Find config manager which will invoke AddMount on changes to Mounts in config file
	gateway.AddMount(&ntripmqtt.Mount{Name: "TEST00AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntripmqtt.Mount{Name: "TEST01AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntripmqtt.Mount{Name: "TEST02AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntripmqtt.Mount{Name: "TEST03AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntripmqtt.Mount{Name: "TEST04AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})
	gateway.AddMount(&ntripmqtt.Mount{Name: "TEST05AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"})

	log.Fatal(gateway.Serve())
}
