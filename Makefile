
all: compile

c_src/wait.o: c_src/async_nif.h
	touch c_src/wait.c

compile: c_src/wait.o
	./rebar compile

test: all
	erl -pa ebin -eval 'wait:run(10000, 30000, 5, 10000).'

clean:
	./rebar clean

# erl -pa ebin -eval 'wait:run(100,30000, 5, 10000), q().'