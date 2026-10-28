.PHONY: release gleam-backend test

gleam-backend:
	cd web/backend && gleam build --target erlang

release: gleam-backend
	rebar3 release
	rm -f _build/default/rel/eTodo/etc/etodo.toml
	if [ -f config/etodo.toml ]; then \
		mkdir -p _build/default/rel/eTodo/etc && \
		cp config/etodo.toml _build/default/rel/eTodo/etc/etodo.toml; \
	fi

test: gleam-backend
	rebar3 eunit
