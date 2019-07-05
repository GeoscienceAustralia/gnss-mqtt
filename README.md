## MQTT for the streaming of GNSS data

One of the benefits of using MQTT is that we can use persistent messages to reduce the duplication of information sent. With RTCM for example, Message 1033 
is sent every 30 seconds so that a client can connect at any time and get an up to date Receiver and Antenna information. Messages such as these can be sent 
as retained messages only when their values change, so that a client will get them when they connect and when they change. In all of my current testing I'm 
not realizing this efficiency due to the fact that we're still dealing with NTRIP streams from receivers.

Could potentially subscribe to all topics on the broker with "#" or maybe "\*/RTCM/#" and generate sourcetable statically (or even from a mix of the geodesyml 
database and stream contents). 


### ntrip-mqtt-gateway

ntrip-mqtt-gateway is an NTRIP server which proxies NTRIP (HTTP) RTCM streams through to MQTT. The library code for this is in pkg/ntripmqtt and the executable 
is in cmd/ntrip-mqtt-gateway.

The ntrip-mqtt-relay agent connects to an NTRIP stream as a client and forwards the data through to an MQTT broker as a publisher; mqtt-ntrip-relay does the 
opposite.
