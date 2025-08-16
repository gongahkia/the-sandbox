all: run

build:
	sudo apt-get install jq
	sudo apt install apt-transport-https curl gnupg
	sudo sh -c 'curl https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -'
	sudo sh -c 'curl https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list'
	sudo apt-get update
	sudo apt-get upgrade
	sudo apt-get autoremove
	sudo apt-get install dart
	dart --version
	chmod +x loader.sh
	dart pub cache repair
	dart pub get

run:
	dart run main.dart | tee inter.json
	tail -n 1 inter.json > sanitised.json
	rm -rf inter.json
	./loader.sh sanitised.json

clean:	
	rm -rf httpCatsDownload log.txt inter.json sanitised.json
