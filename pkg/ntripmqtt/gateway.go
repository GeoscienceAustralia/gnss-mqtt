package ntripmqtt

import (
	"context"
	"fmt"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	log "github.com/sirupsen/logrus"
	"net/http"
	"strings"
	"time"
)

// Gateway receives NTRIP connections and forwards to MQTT
type Gateway struct {
	Port       string
	Hostname   string
	Identifier string
	Operator   string
	//	NMEA       bool
	Country string
	//	Latitude   float64
	//	Longitude  float64
	//	Fallback   string
	//	FallbackIP string
	//	Misc       string
	Mounts     map[string]*Mount
	Broker     string
	MQTTClient mqtt.Client
}

// NewGateway constructs a gateway object, adding a MQTT client
func NewGateway(port, broker string) (gateway *Gateway, err error) {
	mqttClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(broker))
	if connToken := mqttClient.Connect(); connToken.Wait() && connToken.Error() != nil {
		return gateway, connToken.Error()
	}

	gateway = &Gateway{
		Port:       port,
		Mounts:     map[string]*Mount{},
		Broker:     broker,
		MQTTClient: mqttClient,
	}

	// This could probably wait until Serving - Assumes topic structure of "<mount_name>/<message_number>"
	subToken := gateway.MQTTClient.Subscribe("#", 1, func(_ mqtt.Client, msg mqtt.Message) {
		name := strings.Split(msg.Topic(), "/")[0]
		// It's possible for this to cause data races on write, should add a RWMutex for access to Mounts
		if mount, exists := gateway.Mounts[name]; exists {
			mount.LastMessage = time.Now()
		} else {
			gateway.Mounts[name] = &Mount{Name: name, LastMessage: time.Now()}
		}
	})
	subToken.Wait()

	return gateway, subToken.Error()
}

// String representation of Gateway in NTRIP Sourcetable entry format
func (gateway *Gateway) String() string {
	return fmt.Sprintf("CAS;%s;%s;%s;%s;0;%s;0;0;;",
		gateway.Hostname, gateway.Port, gateway.Identifier, gateway.Operator, gateway.Country)
}

// Serve runs NTRIP server on Gateway.Port
func (gateway *Gateway) Serve() error {
	httpMux := mux.NewRouter()
	httpMux.HandleFunc("/", gateway.GetSourcetable).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", gateway.GetMount).Methods("GET")
	httpMux.HandleFunc("/{mountpoint}", gateway.PostMount).Methods("POST")

	// This is probably fancier than it needs to be, could really just have a GetLogger function so we don't have to cast when pulling out the logger
	// Perhaps a mix of both makes sense, where the UUID is added to context, but you generate a logger from the Request (which includes the context) when you need it
	// One benefit of defining the logger in the Context is that it can be appended to
	httpMux.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			requestID := uuid.New().String()
			w.Header().Add("Request-Id", requestID)

			ctx := context.WithValue(r.Context(), "UUID", requestID)
			logger := log.WithFields(log.Fields{
				"request_id": requestID,
				"path":       r.URL.Path,
				"method":     r.Method,
				"source_ip":  r.RemoteAddr,
			})
			ctx = context.WithValue(ctx, "logger", logger)
			logger.Debug("request received")

			next.ServeHTTP(w, r.WithContext(ctx))
		})
	})

	log.Info("server starting")
	return http.ListenAndServe(":"+gateway.Port, httpMux)
}

// GetSourcetable serves gateway and mount information in NTRIP Sourcetable format
func (gateway *Gateway) GetSourcetable(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "%s\r\n", gateway)
	for _, mount := range gateway.Mounts {
		//TODO: Make timeout configurable and make the following if statement a method of Mount
		if mount.LastMessage.After(time.Now().Add(-time.Second * 3)) {
			fmt.Fprintf(w, "%s\r\n", mount)
		}
	}
	w.(http.Flusher).Flush()
}

// GetMount handles GET requests for Mounts, establishing an MQTT client and
// subscription for each request and streaming the data back to the client
func (gateway *Gateway) GetMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Check if mountpoint exists
	mount, exists := gateway.Mounts[r.URL.Path[1:]]
	if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second*3)) {
		logger.Info("no existing mountpoint")
		w.WriteHeader(http.StatusNotFound)
		return
	}

	// Create MQTT client connection on behalf of HTTP user
	subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(gateway.Broker))
	if token := subClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return // TODO: log error
	}
	defer subClient.Disconnect(100)

	// Subscribe to MQTT topic and forward messages to channel
	data := make(chan []byte)
	token := subClient.Subscribe(mount.Name+"/#", 1, func(client mqtt.Client, msg mqtt.Message) {
		//TODO: Add an encapsulation method which takes []byte
		data <- rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize()
	})
	if token.Wait() && token.Error() != nil {
		logger.Error("MQTT subscription failed - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}

	logger.Info("client connected")
	w.(http.Flusher).Flush() // Return 200

	for {
		select {
		// Read data from MQTT channel and write to HTTP connection
		case d := <-data:
			fmt.Fprintf(w, "%s\r\n", d)
			w.(http.Flusher).Flush()
		case <-r.Context().Done():
			logger.Info("client disconnected")
			return
		case <-time.After(time.Second * 3):
			logger.Info("timeout reading from MQTT subscription channel")
			return
		}
	}
}

// PostMount handles POST requests to a Mount, parsing the stream as RTCM and
// forwarding to MQTT broker
// TODO: Currently not checking if the mount is in gateway.Mounts or if it's
// currently up, so can have multiple publishers on the same topic. I don't
// know if there's a reasonable way to avoid this besides consulting LastMessage
func (gateway *Gateway) PostMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Create MQTT client connection on behalf of HTTP user
	pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(gateway.Broker))
	if token := pubClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	defer pubClient.Disconnect(100)

	w.Header().Set("Connection", "close")
	w.(http.Flusher).Flush()
	logger.Info("client connected")

	// Scan POSTed data for RTCM messages and publish with MQTT client
	scanner := rtcm3.NewScanner(r.Body)
	rtcmFrame, err := scanner.NextFrame()
	for ; err == nil; rtcmFrame, err = scanner.NextFrame() {
		pubClient.Publish(fmt.Sprintf("%s/%d", r.URL.Path[1:], rtcmFrame.MessageNumber()), 1, false, rtcmFrame.Payload)
	}
	log.Error("stream ended - " + err.Error())
}
