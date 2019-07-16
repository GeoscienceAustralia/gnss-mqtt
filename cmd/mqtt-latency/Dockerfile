# mqtt-latency - build from project root
FROM golang:1.12 AS builder

WORKDIR /
# Download module dependencies first so they are cached
COPY ./go.mod ./go.sum ./
RUN go mod download

# Build statically linked binary
COPY . .
RUN cd cmd/mqtt-latency/mqttlatency.go
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build .


# Copy binary into scratch image
FROM scratch
COPY --from=builder /cmd/mqtt-latency/mqtt-latency /mqtt-latency
ENTRYPOINT ["/mqtt-latency"]
