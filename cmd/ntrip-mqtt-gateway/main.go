package main

import (
	"flag"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// Currently using Viper to parse the config file directly into a Gateway object.
// Could instead have an intermediary Config object for more flexibility in adding
// config items. For example, might be nice to optionally specify the topic of a
// mount (currently it assumes "MOUNT/#"). Might also be nice to specify the source
// broker for a mount as well as the topic (like a relay) - furthermore this could
// use a prefix to define the protocol for relaying mqtt:// or ntrip://

func main() {
	log.SetFormatter(&log.JSONFormatter{})
	log.SetLevel(log.DebugLevel)

	configFile := flag.String("config", "caster.json", "Path to config file")
	flag.Parse()

	gateway := Gateway{
		Mounts: map[string]*Mount{},
	}

	viper.SetConfigFile(*configFile)
	err := viper.ReadInConfig()
	if err != nil {
		panic(err)
	}

	err = viper.Unmarshal(&gateway)
	if err != nil {
		panic(err)
	}

	if err = gateway.ConnectToBroker(); err != nil {
		panic(err)
	}

	log.Fatal(gateway.Serve())
}
