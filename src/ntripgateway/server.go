package main

import (
	"time"
	"context"
	"net/http"
	"github.com/gorilla/mux"
	"github.com/google/uuid"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	log "github.com/sirupsen/logrus"
)

var ( // TODO: Define from config file - would like to find config manager which is capable of invoking a goroutine for each element of list, as well as for new elements added to the list (watcher functions for mounts)
	caster = &Caster{"2101", "go-ntrip.geops.team", "NTRIP Gateway for MQTT", "GA", "AUS", map[string]*Mount{
		"TEST00AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST01AUS": &Mount{Name: "TEST04AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST02AUS": &Mount{Name: "TEST05AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST03AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST04AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST05AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST06AUS": &Mount{Name: "TEST04AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST07AUS": &Mount{Name: "TEST05AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST08AUS": &Mount{Name: "TEST04AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST09AUS": &Mount{Name: "TEST05AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST10AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST11AUS": &Mount{Name: "TEST04AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST12AUS": &Mount{Name: "TEST05AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST13AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST14AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST15AUS": &Mount{Name: "TEST00AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST16AUS": &Mount{Name: "TEST04AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST17AUS": &Mount{Name: "TEST05AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST18AUS": &Mount{Name: "TEST04AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
		"TEST19AUS": &Mount{Name: "TEST05AUS", Identifier: "Canberra (ACT)", Format: "RTCM 3.3"},
	}, "tcp://localhost:1883"}
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})

	// Watcher subscriptions update last received message on Mount objects so 404s and timeouts can be implemented
	// TODO: Create these subscriptions on initialization of / changes to config
	mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(caster.Broker))
	if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}
	defer mqttClient.Disconnect(100)

	// There should be no harm in just resubscribing for all mounts on any change to config
	for _, mount := range caster.Mounts {
		go func(mount *Mount) { // This doesn't need to be a go routine, but mount does need to be copied so the anonymous function passed to Subscribe isn't a closure referencing the for loop's mount variable
			token := mqttClient.Subscribe(mount.Name+"/RTCM3/#", 1, func(client mqtt.Client, msg mqtt.Message) {
				//TODO: this can conflict with reads of mount.LastMessage and presumably cause runtime errors
				mount.LastMessage = time.Now()
			})
			if token.Wait() && token.Error() != nil {
				panic(token.Error()) //TODO: handle properly
			}
		}(mount)
	}

	httpMux := mux.NewRouter()
	httpMux.HandleFunc("/", caster.GetSourcetable).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", caster.GetMount).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", caster.PostMount).Methods("POST")

	// This is probably fancier than it needs to be, could really just have a GetLogger function so we don't have to cast when pulling out the logger
	// Perhaps a mix of both makes sense, where the UUID is added to context, but you generate a logger from the Request (which includes the context) when you need it
	// One benefit of defining the logger in the Context is that it can be appended to
	httpMux.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			requestId := uuid.New().String()
			ctx := context.WithValue(r.Context(), "UUID", requestId)
			ctx = context.WithValue(ctx, "logger", log.WithFields(log.Fields{
				"request_id": requestId,
				"path":       r.URL.Path,
				"method":     r.Method,
				"source_ip":  r.RemoteAddr,
			}))
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	})

	log.Fatal(http.ListenAndServe(":"+caster.Port, httpMux))
}
