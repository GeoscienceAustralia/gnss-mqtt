FROM golang:1.12 AS builder
WORKDIR /
COPY . .
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build .

FROM scratch
COPY --from=builder /ntripgateway /ntripgateway
EXPOSE 2101
CMD ["/ntripgateway"]
