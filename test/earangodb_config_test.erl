-module(earangodb_config_test).

-include_lib("eunit/include/eunit.hrl").

earangodb_config_test_() ->
    {
        foreach,
        fun() -> persistent_term:erase(earangodb_config) end,
        fun(_) -> application:stop(earangodb) end,
        [
            {"when not configured app fails to start", fun() ->
                when_not_configured_app_fails_to_start()
            end},
            {"application starts with correct config credentials", fun() ->
                application_starts_with_correct_config_credentials()
            end},
            
            {"application fails to start with incorrect config format", fun() ->
                application_fails_to_start_with_incorrect_config_format()
            end}
        ]
    }.

when_not_configured_app_fails_to_start() ->
    ?assertMatch({error, _}, application:ensure_all_started(earangodb)).

application_starts_with_correct_config_credentials() ->
    set_test_config(<<"root">>, <<"test">>),
    ?assertMatch({ok, _}, application:ensure_all_started(earangodb)).


application_fails_to_start_with_incorrect_config_format() ->
    persistent_term:put(earangodb_config, #{}),
    ?assertMatch({error, _}, application:ensure_all_started(earangodb)).

set_test_config(User, Password) ->
    Config = #{
        user => User,
        password => Password,
        url => "http://localhost",
        port => 8529
    },
    persistent_term:put(earangodb_config, Config).
