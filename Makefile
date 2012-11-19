all:
	rm c_src/wait.o; CC=gcc CFLAGS=-fnested-functions ./rebar clean compile
