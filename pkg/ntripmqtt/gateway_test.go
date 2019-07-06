package ntripmqtt_test

import (
	"github.com/surgemq/surgemq/service"
	"github.com/geoscienceaustralia/gnss-mqtt/pkg/ntripmqtt"
	"testing"
)

//TODO: Write tests
func TestGateway(t *testing.T) {
	svr := &service.Server{
		KeepAlive:        300,
		ConnectTimeout:   2,
		SessionsProvider: "mem",
		Authenticator:    "mockSuccess",
		TopicsProvider:   "mem",
	}
	go svr.ListenAndServe("tcp://:1883")
	defer svr.Close()

	// TODO: Mock mqtt broker
	_, err := ntripmqtt.NewGateway("2101", "tcp://localhost:1883")
	if err != nil {
		t.Error(err)
	}
}
