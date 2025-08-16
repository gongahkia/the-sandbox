all:config build

config:
	@echo "configuring..."
	@sudo apt upgrade && sudo apt update && sudo apt autoremove
	@sudo apt install perl
	@perl -v
	@cpan install Term::ReadKey

build:
	@echo "running...";
	@perl snake.perl