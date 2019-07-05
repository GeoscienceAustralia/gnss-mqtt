Make sure auth plugin is built

```
$ cd vernemq_demo_plugin
$ rebar3 compile
```

Inside docker-compose.yml, make sure `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `COGNITO_USER_POOL_ID` and `COGNITO_CLIENT_ID` are configured with appropriate values.

Run VerneMQ configured to use auth plugin

```
$ docker-compose up
```

Test connection to MQTT fails without proper authorisation (unless `DOCKER_VERNEMQ_ALLOW_ANONYMOUS=off` is set, it will just allow)

```
$ mosquitto_pub -h localhost -p 1883 -t test -m test
Connection Refused: not authorised.
Error: The connection was refused.
$ mosquitto_pub -h localhost -p 1883 -t test -m test -u nonexistent -P DoesN0tEx!st
Connection Refused: not authorised.
Error: The connection was refused.
```

Test connecting to MQTT stream with a user that exists within AWS Cognito

```
$ mosquitto_pub -h localhost -p 1883 -t test -m test -u jonathan -P P@ssword1
$
```
