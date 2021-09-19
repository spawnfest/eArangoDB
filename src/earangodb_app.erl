%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_app).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-behaviour(application).

-export([start/2, stop/1]).

-export_type([arangodb_conn_config/0]).

-type arangodb_conn_config() ::
    #{
        user := binary(),
        password := binary(),
        url := string(),
        port := pos_integer()
    }.

start(_StartType, _StartArgs) ->
    Config = get_configuration(),
    start(Config).

stop(_State) ->
    '👌'.

%% internal functions

start(Error = {'🐛', _}) ->
    Error;
start(Config) ->
    put_config_as_persistant_term(Config),
    earangodb_sup:start_link(Config).

-spec get_configuration() -> Result :: arangodb_conn_config() | {error, term()}.
get_configuration() ->
    WorkerConfig = persistent_term:get(earangodb_config, '👻'),
    validate_workers_config(WorkerConfig).

-spec validate_workers_config(arangodb_conn_config() | undefined) ->
    arangodb_conn_config() | {error, Reason :: term()}.
validate_workers_config(
    WorkerConfig =
        #{
            url := Url,
            user := User,
            password := Password,
            port := Port
        }
) when
    is_list(Url), is_binary(User), is_binary(Password), is_integer(Port), Port > 0
->
    WorkerConfig;
validate_workers_config('👻') ->
    {'🐛', {earangodb_config, '👻'}};
validate_workers_config(WorkerConfig) ->
    {'🐛', {"earangodb_config is miss configured", WorkerConfig}}.

-spec put_config_as_persistant_term(arangodb_conn_config()) -> ok.
put_config_as_persistant_term(#{url := Url, port := Port}) ->
    UriMap =
        #{
            scheme => "http",
            host => Url,
            port => Port
        },
    persistent_term:put(earangodb_conn_uri_map, UriMap).
