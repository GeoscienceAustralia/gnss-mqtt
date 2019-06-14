## MQTT for the streaming of GNSS data

Currently httpToMqtt is the prevailing idea - it serves as a proxy which allows NTRIP clients to connect to a stream which is managed by MQTT in the backend.

One of the benefits of using MQTT is that we can use persistent messages to reduce the duplication of information sent. With RTCM for example, Message 1033 
is sent every 30 seconds so that a client can connect at any time and get an up to date Receiver and Antenna information. Messages such as these can be sent 
as retained messages only when their values change, so that a client will get them when they connect and when they change. In all of my current testing I'm 
not realizing this efficiency due to the fact that we're still dealing with NTRIP streams from receivers.
