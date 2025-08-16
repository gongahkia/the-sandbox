CC = gcc
CFLAGS = -g -Wall `sdl2-config --cflags`
LDFLAGS = `sdl2-config --libs`
SOURCES = spinning_polygon.c
EXECUTABLE = spinning_polygon

all: install $(EXECUTABLE)

install:
	sudo apt-get update
	sudo apt-get install -y libsdl2-dev

$(EXECUTABLE): $(SOURCES)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(EXECUTABLE)
