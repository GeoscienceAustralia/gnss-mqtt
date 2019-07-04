# ntrip-mqtt-gateway
FROM golang:1.12 AS builder

WORKDIR /
# Download module dependencies first so they are cached
COPY ./go.mod ./go.sum ./
RUN go mod download

# Build statically linked binary
COPY . .
RUN cd cmd/ntrip-mqtt-gateway
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build .


# Copy binary into scratch image
FROM scratch
COPY --from=builder /cmd/ntrip-mqtt-gateway/ntrip-mqtt-gateway /ntrip-mqtt-gateway
EXPOSE 2101
CMD ["/ntrip-mqtt-gateway"]
