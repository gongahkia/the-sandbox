all:build run clean

run:main
	@echo "running..."
	@./main

build:src/main.go
	@echo "building..."
	@go mod tidy
	@go build src/main.go

config:
	@echo "configuring project..."
	@sudo apt update && sudo apt upgrad && sudo apt autoremove

clean:
	@echo "cleaning..."
	@rm -rf main