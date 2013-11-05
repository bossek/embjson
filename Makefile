PLT_FILE=.embjson.plt

all: compile

compile:
	rebar compile

clean:
	rebar clean

test:
	rebar eunit

$(PLT_FILE):
	dialyzer --output_plt $(PLT_FILE) --build_plt --apps kernel stdlib syntax_tools erts compiler crypto eunit

dialyze: $(PLT_FILE)
	dialyzer --plt $(PLT_FILE) --src src/*.erl
