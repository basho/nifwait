
CFLAGS=-std=gnu99

all:
	rm c_src/wait.o; CC=gcc rebar clean compile
