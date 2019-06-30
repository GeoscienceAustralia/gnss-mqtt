package ntrip_test

import (
	"github.com/geoscienceaustralia/gnss-mqtt/pkg/ntrip"
	"testing"
)

//TODO: Write tests
func TestGateway(t *testing.T) {
	// TODO: Mock mqtt broker
	_, err := ntrip.NewGateway("2101", "", "", "", "", "tcp://localhost:1883")
	if err != nil {
		t.Error(err)
	}
}
