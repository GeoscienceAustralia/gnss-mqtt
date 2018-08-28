In different terminals, run following commands:

1) Start the broker

`$ mosquitto`

2) Start the command line subscriber

`$ mosquitto_sub -v -t 'test'`

3) Publish test message with the command line publisher:

`$ mosquitto_pub -t 'topic' -m 'hello'`

or using hs-mqtt:

`$ ghc rtcmMqtt.hs`

```
$ curl -f --no-buffer --connect-timeout 10 --silent \
    -H "Ntrip-Version: Ntrip/1.0" \
    -H "User-Agent: NTRIP CURL_NTRIP_TEST/0.1" \
    -u username:password  http://auscors.ga.gov.au:2101/PARK4 \
    --output /dev/stdout \
        | ./rtcmMqtt
```

N.B. rtcmMqtt publishes to a different topic for each message type (rtcm3/1077, rtcm3/1087, ...) 
