earangodb
=====

# CI Status

![Erlang CI](https://github.com/spawnfest/eArangoDB/actions/workflows/erlang.yml/badge.svg)
[![codecov](https://codecov.io/gh/spawnfest/eArangoDB/branch/master/graph/badge.svg?token=DWQ81R4OVU)](https://codecov.io/gh/spawnfest/eArangoDB)

## Description

It aims to implement easy communication with ArangoDB database using its [HTTP API](https://www.arangodb.com/docs/stable/http/). This library is written in Erlang so that it can be used from both Erlang and Elixir.

### Documentation

Exdoc documentation is provided for this project [here](https://spawnfest.org/eArangoDB).

## Installation

To `rebar.config` add:

```erlang
{deps, [
    ....
    {earangodb, ".*", {git, "git://github.com/spawnfest/eArangoDB.git", {branch, "master"}}}
]}.
```

### Configuration

There is one parameter to be configured, which is a map containing self explanatory keys: url, port, user and password.

 To configure the earangodb connection, put this parameter in persistent term storage, as shown below.

In `your_project_app.erl` in `start/2` just put:

```erlang
start(_StartType, _StartArgs) ->
    ...
    EarangodbWorkerConfig = #{
        url => "localhost",
        port => 8529,
        user => <<"root">>,
        password => <<"test">>
    },
    persistent_term:put(earangodb_config, EarangodbWorkerConfig),
    {ok, _} = application:ensure_all_started(earangodb),
    ...
```

You may alternatively put this code under a separate supervisor's init.

# DEMO

To see the demo use case of this lib by going to: `demo/demo_cities/README.md`