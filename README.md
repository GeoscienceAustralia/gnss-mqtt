In different terminals, run following commands:

1) Start the broker

`$ mosquitto`

2) Start the command line subscriber

`$ mosquitto_sub -v -t 'test'`

3) Publish test message with the command line publisher:

`$ mosquitto_pub -t 'topic' -m 'hello'`

or using hs-mqtt:

`$ ghc gnss.hs`

`$ ./gnss`
