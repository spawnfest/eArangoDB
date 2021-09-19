-module(earangodb_config_test).

-include_lib("eunit/include/eunit.hrl").

-import(earangodb_config_test_helper, [
    set_test_config/2,
    set_test_config/3
]).

earangodb_config_test_() ->
    {
        foreach,
        fun() -> persistent_term:erase(earangodb_config) end,
        fun(_) -> application:stop(earangodb) end,
        [
            {"when not configured app fails to start",
                fun when_not_configured_app_fails_to_start/0},
            {"application starts with correct config credentials",
                fun application_starts_with_correct_config_credentials/0},
            {"application fails to start with incorrect config format",
                fun application_fails_to_start_with_incorrect_config_format/0},
            {"application fails to start with incorrect config password",
                fun application_fails_to_start_with_incorrect_config_password/0},
            {"application_fails_to_start_with_incorrect_config_port",
                fun application_fails_to_start_with_incorrect_config_port/0},
            {"token_bearer_token_update", fun token_bearer_token_update/0},
            {"token_bearer_logs_unexpected_calls", fun token_bearer_logs_unexpected_calls/0},
            {"token_bearer_logs_unexpected_casts", fun token_bearer_logs_unexpected_casts/0}
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

application_fails_to_start_with_incorrect_config_password() ->
    set_test_config(<<"root">>, <<"wrong_password">>),
    ?assertMatch({error, _}, application:ensure_all_started(earangodb)),
    application:stop(earangodb).

application_fails_to_start_with_incorrect_config_port() ->
    set_test_config(<<"root">>, <<"test">>, 8888),
    ?assertMatch({error, _}, application:ensure_all_started(earangodb)),
    application:stop(earangodb).

token_bearer_token_update() ->
    set_test_config(<<"root">>, <<"test">>),
    ?assertMatch({ok, _}, application:ensure_all_started(earangodb)),
    Token1 = earangodb_token_bearer:get_jwt_token(),
    ?assert(is_pid(whereis(earangodb_token_bearer))),
    whereis(earangodb_token_bearer) ! refresh_jwt_token,
    timer:sleep(100),
    Token2 = earangodb_token_bearer:get_jwt_token(),
    ?assertEqual(Token1, Token2),
    application:stop(earangodb).

token_bearer_logs_unexpected_calls() ->
    set_test_config(<<"root">>, <<"test">>),
    ?assertMatch({ok, _}, application:ensure_all_started(earangodb)),
    logger:set_application_level(earangodb, warning),
    ok = gen_server:call(earangodb_token_bearer, any),
    application:stop(earangodb).

token_bearer_logs_unexpected_casts() ->
    set_test_config(<<"root">>, <<"test">>),
    ?assertMatch({ok, _}, application:ensure_all_started(earangodb)),
    ok = gen_server:cast(earangodb_token_bearer, any),
    application:stop(earangodb).
