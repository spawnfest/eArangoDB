all: setup_db compile format

setup_db:
	docker-compose up -d

compile:
	rebar3 compile

format:
	rebar3 fmt
