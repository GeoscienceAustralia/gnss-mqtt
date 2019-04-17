## MQTT for the streaming of GNSS data

Currently httpToMqtt is the prevailing idea - it serves as a proxy which allows NTRIP clients to connect to a stream which is managed by MQTT in the backend.

One of the benefits of using MQTT is that we can use persistent messages to reduce the duplication of information sent. With RTCM for example, Message 1033 
is sent every 30 seconds so that a client can connect at any time and get an up to date Receiver and Antenna information. Messages such as these can be sent 
as retained messages only when their values change, so that a client will get them when they connect and when they change. In all of my current testing I'm 
not realizing this efficiency due to the fact that we're still dealing with NTRIP streams from receivers.

There is currently an effort to standardize the topic structure for the transmission of RTCM data via MQTT. This project is currently using the structure: 
`/<stationId>/<messageNumber>`. This will almost certainly need to change to something like: `/<stationId>/<rtcmVersion>/<messageNumber>`. Given that RTCM3 
is strictly additive in it's message definitions, it's not particularly useful to mention the minor versions in the topic structure - and so perhaps the 
topic structure standard can be specifically for RTCM3 (and not necessarily any past or future versions). However that's a much longer conversation to come. 
For the purposes of testing the general usefulness of streaming data using MQTT, any topic structure will do.
