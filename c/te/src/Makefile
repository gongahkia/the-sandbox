all:
	run

run:
	./a.out

up:
	git pull

configure:
	sudo apt-get upgrade && sudo apt-get update && sudo apt-get autoremove
	sudo apt install gcc 
	sudo apt install ncurses

build:
	gcc main.c -lncurses

clean:
	rm -rf .git .gitignore README.md a.out
