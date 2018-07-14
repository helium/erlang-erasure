.PHONY: compile rel test typecheck

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eqc -n 100

typecheck:
	$(REBAR) dialyzer

doc:
	$(REBAR) edoc
