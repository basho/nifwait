
CFLAGS=-std=gnu99

all:
	CC=gcc CFLAGS="-std=gnu99 -Wstrict-prototypes" ./rebar clean compile
