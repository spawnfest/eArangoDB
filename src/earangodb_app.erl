%%%-------------------------------------------------------------------
%% @doc earangodb public API
%% @end
%%%-------------------------------------------------------------------

-module(earangodb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    earangodb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
