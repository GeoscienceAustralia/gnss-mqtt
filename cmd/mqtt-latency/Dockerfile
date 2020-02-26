# mqtt-latency - build from project root
FROM golang:1.13 AS builder

WORKDIR /
# Download module dependencies first so they are cached
COPY ./go.mod ./go.sum ./
RUN go mod download

# Build statically linked binary
COPY . .
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build cmd/mqtt-latency/mqtt-latency.go


# Copy binary into scratch image
FROM scratch
COPY --from=builder /mqtt-latency /mqtt-latency
ENTRYPOINT ["/mqtt-latency"]
