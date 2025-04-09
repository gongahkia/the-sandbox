all:build

build:main.go
	@go mod tidy
	@go build main.go

examples:
	@git clone github.com/sjwhitworth/golearn
	@cd examples/knnclassifier && go run knnclassifier_iris.go
	@cd examples/instances && go run instances.go
	@cd examples/trees && go run trees.go

clean:
	@rm -rf golearn