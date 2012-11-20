
all:
	CFLAGS="-std=c99" ./rebar clean compile

test: all
	erl -pa ebin -eval 'wait:run(100,30000, 5, 10000).'

clean:
	./rebar clean

# erl -pa ebin -eval 'wait:run(100,30000, 5, 10000), q().'