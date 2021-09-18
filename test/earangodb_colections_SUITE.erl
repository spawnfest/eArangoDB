-module(earangodb_colections_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

init_per_suite(Config) ->
    set_test_config(),
    {ok, _} = application:ensure_all_started(earangodb),
    Config.

end_per_suite(_Config) ->
    application:stop(earangodb),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) -> ok.

all() ->
    [
        colections_list_returns_list_containing_colections_id
    ].

colections_list_returns_list_containing_colections_id(_Config) ->
    [#{<<"id">> := _} | _] = earangodb:collections_list().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_test_config() ->
    Config = #{
        user => <<"root">>,
        password => <<"test">>,
        url => "localhost",
        port => 8529
    },
    persistent_term:put(earangodb_config, Config).
