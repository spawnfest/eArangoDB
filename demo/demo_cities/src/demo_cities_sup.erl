%%%-------------------------------------------------------------------
%% @doc demo_cities top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(demo_cities_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    EarangodbWorkerConfig = #{
        url => "localhost",
        port => 8529,
        user => <<"root">>,
        password => <<"test">>
    },
    persistent_term:put(earangodb_config, EarangodbWorkerConfig),
    {ok, _} = application:ensure_all_started(earangodb),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
