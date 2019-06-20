package main

import (
	"fmt"
	"time"
	"net/http"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	log "github.com/sirupsen/logrus"
)

// Caster contains global configuration for handling connections and constructing sourcetable
type Caster struct {
	Port       string
	Hostname   string // TODO: Lookup?
	Identifier string
	Operator   string
	//	NMEA       bool
	Country string
	//	Latitude   float64
	//	Longitude  float64
	//	Fallback   string
	//	FallbackIP string
	//	Misc       string
	Mounts map[string]*Mount
	Broker string
}

// String representation of Caster in NTRIP Sourcetable entry format
func (caster *Caster) String() string {
	return fmt.Sprintf("CAS;%s;%s;%s;%s;0;%s;0;0;;",
		caster.Hostname, caster.Port, caster.Identifier, caster.Operator, caster.Country)
}

// GetSourcetable serves caster and mount information in NTRIP Sourcetable format
func (caster *Caster) GetSourcetable(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "%s\r\n", caster)
	for _, mount := range caster.Mounts {
		//TODO: Make timeout configurable and make the following if statement a method of Mount
		if mount.LastMessage.After(time.Now().Add(-time.Second * 3)) {
			fmt.Fprintf(w, "%s\r\n", mount)
		}
	}
	w.(http.Flusher).Flush()
}

// GetMount handles GET requests for Mounts, establishing an MQTT client and
// subscription for each request and streaming the data back to the client
func (caster *Caster) GetMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Check if mountpoint exists
	mount, exists := caster.Mounts[r.URL.Path[1:]]
	if !exists || mount.LastMessage.Before(time.Now().Add(-time.Second*3)) {
		logger.Info("no existing mountpoint")
		w.WriteHeader(http.StatusNotFound)
		return
	}

	// Create MQTT client connection on behalf of HTTP user
	subClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(caster.Broker))
	if token := subClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return // TODO: log error
	}
	defer subClient.Disconnect(100)

	// Subscribe to MQTT topic and forward messages to channel
	data := make(chan []byte)
	token := subClient.Subscribe(mount.Name+"/RTCM3/#", 1, func(client mqtt.Client, msg mqtt.Message) {
		data <- rtcm3.EncapsulateMessage(rtcm3.DeserializeMessage(msg.Payload())).Serialize() // Need an encapsulation method which takes []byte
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
// TODO: Currently not checking if the mount is in caster.Mounts or if it's
// currently up, so can have multiple publishers on the same topic. I don't
// know if there's a reasonable way to avoid this
func (caster *Caster) PostMount(w http.ResponseWriter, r *http.Request) {
	logger := r.Context().Value("logger").(*log.Entry)

	// Create MQTT client connection on behalf of HTTP user
	pubClient := mqtt.NewClient(mqtt.NewClientOptions().AddBroker(caster.Broker))
	if token := pubClient.Connect(); token.Wait() && token.Error() != nil {
		logger.Error("failed to create MQTT client - " + token.Error().Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	defer pubClient.Disconnect(100)

	logger.Info("client connected")
	w.Header().Set("Connection", "close")
	w.(http.Flusher).Flush()
	// Without this sleep it looks like we're reading from the body before the POSTer has sent any data, which returns an error
	// Need to find a better way of waiting until we have received data / timing out
	time.Sleep(1 * time.Second)

	// Scan POSTed data for RTCM messages and publish with MQTT client
	scanner := rtcm3.NewScanner(r.Body)
	rtcmFrame, err := scanner.NextFrame()
	for ; err == nil; rtcmFrame, err = scanner.NextFrame() {
		pubClient.Publish(fmt.Sprintf("%s/RTCM3/%d", r.URL.Path[1:], rtcmFrame.MessageNumber()), 1, false, rtcmFrame.Payload)
	}
	log.Error("stream ended - " + err.Error())
}
