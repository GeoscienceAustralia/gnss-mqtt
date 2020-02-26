# ntrip-mqtt-relay - build from project root
FROM golang:1.13 AS builder

WORKDIR /
# Download module dependencies first so they are cached
COPY ./go.mod ./go.sum ./
RUN go mod download

# Build statically linked binary
COPY . .
RUN CD cmd/ntrip-mqtt-relay/
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build .


# Copy binary into scratch image
FROM scratch
COPY --from=builder /cmd/ntrip-mqtt-relay/ntrip-mqtt-relay /ntrip-mqtt-relay
EXPOSE 2101
ENTRYPOINT ["/ntrip-mqtt-relay"]
