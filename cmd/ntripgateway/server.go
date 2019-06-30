package main

import (
	log "github.com/sirupsen/logrus"
	"github.com/geoscienceaustralia/gnss-mqtt/ntrip"
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})

	// TODO: Define from config file - would like to find config manager which is capable of invoking a goroutine for each element of list, as well as for new elements added to the list (watcher functions for mounts)
	gateway := &ntrip.Gateway{"2101", "go-ntrip.geops.team", "NTRIP Gateway for MQTT", "GA", "AUS", map[string]*ntrip.Mount{
		"TEST00AUS0": &ntrip.Mount{Name: "TEST00AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST01AUS0": &ntrip.Mount{Name: "TEST01AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST02AUS0": &ntrip.Mount{Name: "TEST02AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST03AUS0": &ntrip.Mount{Name: "TEST03AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST04AUS0": &ntrip.Mount{Name: "TEST04AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST05AUS0": &ntrip.Mount{Name: "TEST05AUS0", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
	}, "tcp://localhost:1883"}

	gateway.Serve()
}
