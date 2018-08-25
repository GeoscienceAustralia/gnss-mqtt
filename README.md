Start the broker:

`mosquitto`

Start the command line subscriber:

`mosquitto_sub -v -t 'test/topic'`

Publish test message with the command line publisher:

`mosquitto_pub -t 'test/topic' -m 'helloWorld'`
