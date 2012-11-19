
all:
	rm c_src/wait.o; CC=gcc rebar clean compile
