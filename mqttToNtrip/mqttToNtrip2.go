package main

import (
	"errors"
	"flag"
	"fmt"
	mqtt "github.com/eclipse/paho.mqtt.golang"
	"github.com/geoscienceaustralia/go-rtcm/rtcm3"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"
	"sync"
)

type Servers struct {
	sync.RWMutex
	servers  map[string]*Server
	Caster   string
	Username string
	Password string
}

func (s *Servers) GetServer(mount string) (server *Server) {
	s.Lock()
	defer s.Unlock()
	server, exists := s.servers[mount]
	if !exists {
		server, _ = NewServer(s.Caster + mount)
		server.SetBasicAuth(s.Username, s.Password)
		s.servers[mount] = server
		go s.Serve(server, mount)
	}
	return server
}

func (s *Servers) Serve(server *Server, mount string) {
	defer s.DeleteServer(mount)
	fmt.Println("connecting", mount)
	resp, err := server.Connect()
	fmt.Println(mount, resp, err)
	fmt.Println("disconnecting", mount)
}

func (s *Servers) DeleteServer(mount string) {
	s.Lock()
	delete(s.servers, mount)
	s.Unlock()
}

func main() {
	broker := flag.String("broker", "tcp://localhost:1883", "MQTT broker")
	caster := flag.String("caster", "http://localhost:2101/", "NTRIP caster")
	username := flag.String("username", "", "NTRIP username")
	password := flag.String("password", "", "NTRIP password")
	flag.Parse()

	servers := &Servers{
		servers:  make(map[string]*Server),
		Caster:   *caster,
		Username: *username,
		Password: *password,
	}

	opts := mqtt.NewClientOptions()
	opts.AddBroker(*broker)
	opts.SetDefaultPublishHandler(func(client mqtt.Client, msg mqtt.Message) {
	})
	client := mqtt.NewClient(opts)
	if token := client.Connect(); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	if token := client.Subscribe("#", 1, nil); token.Wait() && token.Error() != nil {
		panic(token.Error())
	}

	select {}
}

type Server struct {
	*http.Request
	writer *io.PipeWriter
}

func NewServer(ntripCasterUrl string) (server *Server, err error) {
	u, err := url.Parse(ntripCasterUrl)
	server = &Server{
		Request: &http.Request{
			URL:              u,
			Method:           "POST",
			ProtoMajor:       1,
			ProtoMinor:       1,
			TransferEncoding: []string{"chunked"},
			Header:           make(map[string][]string),
		},
	}
	server.Header.Set("User-Agent", "NTRIP GoClient")
	server.Header.Set("Ntrip-Version", "Ntrip/2.0")
	return server, err
}

func (server *Server) Connect() (resp *http.Response, err error) {
	reader, writer := io.Pipe()
	server.Request.Body = reader
	server.writer = writer
	resp, err = http.DefaultClient.Do(server.Request)
	if err == nil || resp.StatusCode == 200 {
		ioutil.ReadAll(resp.Body)
	}

	return resp, err
}

func (server *Server) Write(data []byte) (n int, err error) {
	if server.writer == nil {
		return 0, errors.New("not connected")
	}
	return server.writer.Write(data)
}
