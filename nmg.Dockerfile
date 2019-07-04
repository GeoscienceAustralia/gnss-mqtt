# ntrip-mqtt-gateway
FROM golang:1.12 AS builder

WORKDIR /
# Download module dependencies first so they are cached
COPY ./go.mod ./go.sum ./
RUN go mod download

# Build statically linked binary
COPY . .
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build ./cmd/ntrip-mqtt-gateway/


# Copy binary into scratch image
FROM scratch
COPY --from=builder /ntrip-mqtt-gateway /ntrip-mqtt-gateway
EXPOSE 2101
CMD ["/ntrip-mqtt-gateway"]
