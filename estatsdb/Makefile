ERL ?= erl
APP := estatsdb

.PHONY: deps

all: deps
	@./rebar compile

release: all
	@./rebar --force generate

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test: all
	@./rebar skip_deps=true eunit

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
