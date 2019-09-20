package main

import (
	"flag"
	log "github.com/sirupsen/logrus"
)

func main() {
	port := flag.String("ntrip-port", "2101", "")
	broker := flag.String("mqtt-broker", "tcp://localhost:1883", "")
	flag.Parse()

	log.SetFormatter(&log.JSONFormatter{})
	log.SetLevel(log.DebugLevel)

	// TODO: Define from config file
	gateway, err := NewGateway(*port, *broker)
	if err != nil {
		log.Fatal(err)
	}

	gateway.Hostname = "streams.geops.team"
	gateway.Operator = "Geoscience Australia"

	log.Fatal(gateway.Serve())
}
