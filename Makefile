PROJECT   = stdlib2

# Options
ERLC_OPTS = +debug_info +nowarn_shadow_vars +warnings_as_errors

# Standard targets
include erlang.mk

.PHONY: eunit
eunit:
	erl -noshell -pa ebin -eval 'eunit:test("ebin", [verbose])' -s init stop

# eof
