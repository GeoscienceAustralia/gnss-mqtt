# ntrip-mqtt-gateway - build from project root
FROM golang:1.13 AS builder

WORKDIR /build
# Download module dependencies first so they are cached
COPY ./go.mod ./go.sum ./
RUN go mod download

# Build statically linked binary
COPY . .
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o ntrip-mqtt-gateway ./cmd/ntrip-mqtt-gateway

# Copy binary into scratch image
FROM scratch
COPY --from=builder /build/ntrip-mqtt-gateway /ntrip-mqtt-gateway
EXPOSE 2101
ENTRYPOINT ["/ntrip-mqtt-gateway"]
