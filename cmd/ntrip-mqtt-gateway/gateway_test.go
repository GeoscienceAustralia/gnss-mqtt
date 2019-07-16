package main

import (
	"github.com/surgemq/surgemq/service"
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
	_, err := NewGateway("2101", "tcp://localhost:1883")
	if err != nil {
		t.Error(err)
	}
}
