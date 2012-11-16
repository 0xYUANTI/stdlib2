# Standard Erlang Makefile

REBAR=./rebar
PLT=./.plt
suite=$(if $(SUITE), suite=$(SUITE), )

# Basics ###############################################################
.PHONY: all deps compile get-deps update-deps doc clean distclean

all: deps compile

deps: get-deps update-deps

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

doc:
	$(REBAR) doc

clean:
	$(REBAR) clean
	$(RM) doc/*

distclean: clean
	$(REBAR) delete-deps

# Utilities ############################################################
.PHONY: repl eunit_repl

repl:
	erl -pa ebin deps/*/ebin

eunit_repl:
	erl -pa .eunit deps/*/ebin

# Test Suite ###########################################################
.PHONY: test eunit xref plt dialyzer

test: eunit

eunit:
	$(REBAR) eunit $(suite) skip_deps=true

xref:
	$(REBAR) xref skip_deps=true

plt: compile
	dialyzer --build_plt --output_plt $(PLT) ebin deps/*/ebin

dialyzer: compile
	dialyzer --plt $(PLT) ebin

# eof
