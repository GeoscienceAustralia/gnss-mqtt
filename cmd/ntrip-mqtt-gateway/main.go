package main

import (
	"flag"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

func main() {
	log.SetFormatter(&log.JSONFormatter{})
	log.SetLevel(log.DebugLevel)

	configFile := flag.String("config", "caster.json", "Path to config file")
	flag.Parse()

	gateway := Gateway{}

	viper.SetConfigFile(*configFile)
	err := viper.ReadInConfig()
	if err != nil {
		panic(err)
	}

	err = viper.Unmarshal(&gateway)
	if err != nil {
		panic(err)
	}

	if err = gateway.Connect(); err != nil {
		panic(err)
	}

	log.Fatal(gateway.Serve())
}
