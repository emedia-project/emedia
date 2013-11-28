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

gen-doc: clean-doc
	@mkdir doc
	@cp _doc/* doc
	@$(REBAR) doc skip_deps=true

clean-doc: doc
	@rm -rf doc

run: get-deps compile
	erl -pa deps/*/ebin -pa ./ebin -mnesia dir '"eMediaTest.mnesia"'

