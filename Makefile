all: setup_db precommit show_cover

precommit: compile format run_type_checks run_tests

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

run_tests:
	rebar3 ct --cover
	rebar3 eunit --cover

show_cover:
	rebar3 cover

report_cover:
	rebar3 codecov analyze

# Requires ex_doc to be installed locally
# As described here: https://github.com/elixir-lang/ex_doc#using-exdoc-with-erlang-projects
build_docs:
	rebar3 edoc
	ex_doc "eArangoDB" "0.1" "_build/default/lib/earangodb/ebin" --paths "_build/default/lib/*/ebin"
	mv doc docs
