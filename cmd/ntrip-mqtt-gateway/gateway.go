package main

import (
	"context"
	"fmt"
	"net/http"
	"strings"
	"time"

	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	"github.com/google/uuid"
	"github.com/gorilla/mux"
	log "github.com/sirupsen/logrus"
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
	// TODO: Should the string in the map be a pointer to the Mount.Name? Probably couldn't use Viper to construct this object directly in that case
	// Could just use a list of Mount objects instead
	Mounts       map[string]*Mount
	MQTTBroker   string
	MQTTUsername string
	MQTTPassword string
	MQTTClient   mqtt.Client
}

// ConnectToBroker as a client and subscribe to all RTCM3 topics - This is used
// to populate Mount.LastMessage, which is used for the Sourcetable and the gives
// ability to serve 404's
func (gateway Gateway) ConnectToBroker() (err error) {
	gateway.MQTTClient = mqtt.NewClient(mqtt.NewClientOptions().
		AddBroker(gateway.MQTTBroker).
		SetClientID(uuid.New().String()).
		SetUsername(gateway.MQTTUsername).
		SetPassword(gateway.MQTTPassword).
		SetMaxReconnectInterval(5 * time.Second).
		SetCleanSession(false))

	if connToken := gateway.MQTTClient.Connect(); connToken.Wait() && connToken.Error() != nil {
		return connToken.Error()
	}

	subToken := gateway.MQTTClient.Subscribe("+/rtcm3/#", 1, func(_ mqtt.Client, msg mqtt.Message) {
		mountName := strings.Split(msg.Topic(), "/")[0]
		// It's possible for this to cause data races on write, should add a RWMutex for access to Mounts
		if closureMount, ok := gateway.Mounts[mountName]; ok {
			closureMount.LastMessage = time.Now()
		} else {
			mount := Mount{Name: mountName, LastMessage: time.Now()}
			gateway.Mounts[mountName] = &mount
		}
	})

	if subToken.Wait(); subToken.Error() != nil {
		log.Error("failed to subscribe to RTCM3 Topics " + subToken.Error().Error())
	}

	log.Info("connected to MQTT Broker")
	return nil
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
	httpMux.Use(LoggingMiddleware)
	httpMux.Use(gateway.AuthenticatorMiddleware)

	log.Info("server starting")
	return http.ListenAndServe(":"+gateway.Port, httpMux)
}

func LoggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		requestID := uuid.New().String()
		w.Header().Add("Request-Id", requestID)

		ctx := context.WithValue(r.Context(), "uuid", requestID)
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
}

func (gateway *Gateway) AuthenticatorMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger := r.Context().Value("logger").(*log.Entry)

		// Skip Authentication for Sourcetable access
		if r.URL.Path == "/" {
			next.ServeHTTP(w, r)
			return
		}

		// Get basic auth
		username, password, ok := r.BasicAuth()
		if !ok {
			logger.Info("no auth provided")
			w.WriteHeader(http.StatusUnauthorized)
			return
		}

		// Create MQTT client connection on behalf of HTTP user
		mqttClient := mqtt.NewClient(mqtt.NewClientOptions().
			AddBroker(gateway.MQTTBroker).
			SetClientID(r.Context().Value("uuid").(string)).
			SetUsername(username).
			SetPassword(password).
			SetCleanSession(false))

		if token := mqttClient.Connect(); token.Wait() && token.Error() != nil {
			logger.Error("failed to create MQTT client - " + token.Error().Error())
			if token.Error().Error() == "Not Authorized" {
				w.WriteHeader(http.StatusUnauthorized)
			} else {
				w.WriteHeader(http.StatusInternalServerError)
			}
			return
		}

		ctx := context.WithValue(r.Context(), "mqttclient", mqttClient)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
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
	subClient := r.Context().Value("mqttclient").(mqtt.Client)

	// TODO: Consider moving this check to the AuthenticatorMiddleware, as it does
	// not hide information because the sourcetable is a public list of available
	// endpoints - saves making a connection to the broker. Could potentially use the
	// same check in the case of PostMount as well.
	mount, exists := gateway.Mounts[strings.ToLower(r.URL.Path[1:])]
	if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second*3)) {
		logger.Info("no existing mountpoint")
		w.WriteHeader(http.StatusNotFound)
		return
	}

	//defer subClient.Disconnect(100)
	data := make(chan []byte)
	token := subClient.Subscribe(r.URL.Path[1:]+"/rtcm3/#", 1, func(client mqtt.Client, msg mqtt.Message) {
		data <- rtcm3.EncapsulateByteArray(msg.Payload()).Serialize()
	})
	if token.Wait() && token.Error() != nil {
		logger.Error("MQTT subscription failed - " + token.Error().Error())
	}

	logger.Info("client connected")
	w.(http.Flusher).Flush() // Return 200

	for {
		select {
		// Read data from MQTT channel and write to HTTP connection
		case d := <-data:
			fmt.Fprintf(w, "%s", d)
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
	pubClient := r.Context().Value("mqttclient").(mqtt.Client)
	//defer pubClient.Disconnect(100)

	w.Header().Set("Connection", "close")
	w.(http.Flusher).Flush()
	logger.Info("client connected")

	// Scan POSTed data for RTCM messages and publish with MQTT client - This will do nothing if the stream does not contain RTCM data
	scanner := rtcm3.NewScanner(r.Body)
	rtcmFrame, err := scanner.NextFrame()
	for ; err == nil; rtcmFrame, err = scanner.NextFrame() {
		pubClient.Publish(fmt.Sprintf("%s/rtcm3/%d", r.URL.Path[1:], rtcmFrame.MessageNumber()), 1, false, rtcmFrame.Payload)
	}
	log.Error("stream ended - " + err.Error())
}
