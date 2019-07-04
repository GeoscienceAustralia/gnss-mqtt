# ntrip-mqtt-gateway
FROM golang:1.12 AS builder
WORKDIR /
COPY . .
RUN go get
RUN cd cmd/ntrip-mqtt-gateway
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build .

FROM scratch
COPY --from=builder /cmd/ntrip-mqtt-gateway/ntrip-mqtt-gateway /ntrip-mqtt-gateway
EXPOSE 2101
CMD ["/ntrip-mqtt-gateway"]
