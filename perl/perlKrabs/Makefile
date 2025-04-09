all:config build

config:
	@echo "configuring..."
	@sudo apt upgrade && sudo apt update && sudo apt autoremove
	@sudo apt install perl
	@perl -v
	@cpan install LWP::UserAgent
	@cpan install URI::Escape

build:
	@read -p "provide a search query: " query;
	@echo "running...";
	@perl main.pl "$$query"