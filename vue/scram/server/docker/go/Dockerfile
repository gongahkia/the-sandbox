FROM golang:1.21-alpine
RUN apk add --no-cache gcc musl-dev
WORKDIR /app
COPY go-runner.sh .
CMD ["go", "run", "go-runner.sh"]