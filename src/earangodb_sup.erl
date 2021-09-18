%%%-------------------------------------------------------------------
%% @doc earangodb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(earangodb_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(_Config) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 5
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
