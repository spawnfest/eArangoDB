-module(earangodb_config_test_helper).

-export([set_test_config/0, set_test_config/2, set_test_config/3]).

set_test_config() ->
    set_test_config(<<"root">>, <<"test">>).

set_test_config(User, Password) ->
    set_test_config(User, Password, 8529).

set_test_config(User, Password, Port) ->
    set_test_config(User, Password, Port, "localhost").

set_test_config(User, Password, Port, Url) ->
    Config = #{
        user => User,
        password => Password,
        url => Url,
        port => Port
    },
    persistent_term:put(earangodb_config, Config).
