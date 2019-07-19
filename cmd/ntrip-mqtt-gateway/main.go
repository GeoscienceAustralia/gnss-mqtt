package main

import (
	log "github.com/sirupsen/logrus"
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})
	log.SetLevel(log.DebugLevel)

	// TODO: Define from config file
	gateway, err := NewGateway("2101", "tcp://mqtt.geops.team:1883")
	if err != nil {
		log.Fatal(err)
	}

	gateway.Hostname = "ntrip.geops.team"
	gateway.Operator = "Geoscience Australia"

	log.Fatal(gateway.Serve())
}
