-module(earangodb_app).

-behaviour(application).

-export([start/2, stop/1]).

-export_type([arangodb_conn_config/0]).

-type arangodb_conn_config() :: #{
    user := binary(),
    password := binary(),
    url := string(),
    port := pos_integer()
}.

start(_StartType, _StartArgs) ->
    Config = get_configuration(),
    start(Config).

stop(_State) ->
    ok.

%% internal functions

start(Error = {error, _}) -> Error;
start(Config) -> earangodb_sup:start_link(Config).

-spec get_configuration() -> Result :: arangodb_conn_config() | {error, term()}.
get_configuration() ->
    WorkerConfig = persistent_term:get(earangodb_config, undefined),
    validate_workers_config(WorkerConfig).

-spec validate_workers_config(arangodb_conn_config() | undefined) ->
    arangodb_conn_config() | {error, Reason :: term()}.
validate_workers_config(
    WorkerConfig = #{url := Url, user := User, password := Password, port := Port}
) when is_list(Url), is_binary(User), is_binary(Password), is_integer(Port), Port > 0 ->
    WorkerConfig;
validate_workers_config(undefined) ->
    {error, {earangodb_config, undefined}};
validate_workers_config(WorkerConfig) ->
    {error, {"earangodb_config is miss configured", WorkerConfig}}.
