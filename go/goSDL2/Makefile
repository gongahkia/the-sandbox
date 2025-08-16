all:build

build:main.go
	@go mod tidy
	@go build main.go

config:
	@sudo apt update && sudo apt upgrade && sudo apt autoremove
	@sudo apt install libsdl2{,-image,-mixer,-ttf,-gfx}-dev