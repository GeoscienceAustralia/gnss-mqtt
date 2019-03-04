FROM golang:1.12.0-alpine as builder
WORKDIR /root/
RUN apk update && apk add --no-cache git ca-certificates && update-ca-certificates
RUN git clone https://github.com/umeat/go-ntrip && \
    cd go-ntrip/cmd/ntripcaster/ && \
        CGO_ENABLED=0 GOOS=linux go build -i -o /root/ntripcaster .

FROM scratch
WORKDIR /root/
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=builder /root/ntripcaster .
ENTRYPOINT ["./ntripcaster"]
