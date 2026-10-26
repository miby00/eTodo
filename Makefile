.PHONY: release gleam-backend test

gleam-backend:
	cd web/backend && gleam build --target erlang

release: gleam-backend
	rebar3 release

test: gleam-backend
	rebar3 eunit
