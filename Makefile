all: setup_db compile format run_type_checks

setup_db:
	docker-compose up -d

compile:
	rebar3 compile

format:
	rebar3 fmt

run_type_checks:
	rebar3 xref
	rebar3 gradualizer
	rebar3 dialyzer
