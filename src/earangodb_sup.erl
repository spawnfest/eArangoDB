%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(Config) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 5
    },
    ChildSpecs = [
        #{
            id => jwt_token_bearer,
            start => {earangodb_token_bearer, start_link, [Config]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
