
CFLAGS=-std=gnu99

all:
	CC=gcc CFLAGS="-std=gnu99 -Wstrict-prototypes" ./rebar clean compile

test:
	erl -pa ebin -eval 'wait:run(100,30000, 5, 10000).'


# erl -pa ebin -eval 'wait:run(100,30000, 5, 10000), q().'