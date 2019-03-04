#!/usr/bin/env bash

# docker build -t ntripcaster - < ntripcaster.Dockerfile

# Generate keys for testing HTTPS
go run $GOROOT/src/crypto/tls/generate_cert.go --rsa-bits 1024 --host 127.0.0.1,::1,localhost --ca --start-date "Jan 1 00:00:00 1970" --duration=1000000h

docker run -d \
    -p 2101-2102:2101-2102/tcp \
    -v `pwd`/caster_config.yml:/root/config/caster_config.yml \
    -v `pwd`/cert.pem:/root/config/cert.pem \
    -v `pwd`/key.pem:/root/config/key.pem \
    -v ~/.aws:/root/.aws \
    -e AWS_REGION="ap-southeast-2" \
    -e AWS_SHARED_CREDENTIALS_FILE="/root/.aws/credentials" \
    ntripcaster -config "/root/config/caster_config.yml"

rm cert.pem key.pem
