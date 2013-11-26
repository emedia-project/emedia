REBAR=$(shell which rebar || echo ./rebar)

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

tests: compile
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean

doc:
	@$(REBAR) doc skip_deps=true

run: get-deps compile
	erl -pa deps/*/ebin -pa ./ebin
