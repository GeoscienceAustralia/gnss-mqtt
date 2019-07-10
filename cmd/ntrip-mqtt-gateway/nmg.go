package main

import (
	"github.com/geoscienceaustralia/gnss-mqtt/pkg/ntripmqtt"
	log "github.com/sirupsen/logrus"
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})
	log.SetLevel(log.DebugLevel)

	// TODO: Define from config file
	gateway, err := ntripmqtt.NewGateway("2101", "tcp://mqtt.geops.team:443")
	if err != nil {
		log.Fatal(err)
	}

	gateway.Hostname = "streams.geops.team"
	gateway.Operator = "Geoscience Australia"

	log.Fatal(gateway.Serve())
}
