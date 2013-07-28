all: deps compile

deps:
	@( ./rebar get-deps )

compile:
	@( ./rebar compile )

.PHONY: all deps compile