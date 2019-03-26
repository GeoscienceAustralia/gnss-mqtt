package main

import (
    "fmt"
    "time"
    "flag"
    "github.com/umeat/go-ntrip/ntrip"
    "github.com/geoscienceaustralia/go-rtcm/rtcm3"
)

func main() {
    source := flag.String("caster", "http://one.auscors.ga.gov.au:2101/ALIC7", "NTRIP caster mountpoint to stream from")
    username := flag.String("username", "", "NTRIP username")
    password := flag.String("password", "", "NTRIP password")
    timeout := flag.Duration("timeout", 2 * time.Second, "NTRIP reconnect timeout")
    flag.Parse()

    ntripClient, _ := ntrip.NewClient(*source)
    ntripClient.SetBasicAuth(*username, *password)

	ntripServer, _ := ntrip.NewServer("http://localhost:2101/TEST")
	ntripServer.Connect()

    for ; ; time.Sleep(time.Second * *timeout) {
        resp, err := ntripClient.Connect()
        if err != nil || resp.StatusCode != 200 {
            fmt.Println("NTRIP client failed to connect -", resp.StatusCode, err)
            continue
        }

        scanner := rtcm3.NewScanner(resp.Body)
        for msg, err := scanner.Next(); err == nil; msg, err = scanner.Next() {
			ntripServer.Write(rtcm3.EncapsulateMessage(msg).Serialize())
        }

        fmt.Println("NTRIP client connection died -", err)
    }
}
