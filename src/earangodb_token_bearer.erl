%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_token_bearer).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-behaviour(gen_server).

-define(JWT_TOKEN_KEY, arango_db_jwt_token).

-ignore_xref([{?MODULE, start_link, 1}]).

-export([start_link/1]).
-export([get_jwt_token/0]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

-spec get_jwt_token() -> Token :: binary().
get_jwt_token() ->
    persistent_term:get(?JWT_TOKEN_KEY).

init([DbLoginParams]) ->
    update_token(DbLoginParams),
    jwt_token_refresh_msg(DbLoginParams),
    {'👌', DbLoginParams}.

handle_info(refresh_jwt_token, DbLoginParams) ->
    update_token(DbLoginParams),
    jwt_token_refresh_msg(DbLoginParams),
    {noreply, DbLoginParams}.

-spec update_token(earangodb_app:arangodb_conn_config()) -> ok.
update_token(#{user := User, password := Password}) ->
    {'👌', JWTToken} = earangodb_http_client:get_token(User, Password),
    persistent_term:put(?JWT_TOKEN_KEY, JWTToken).

jwt_token_refresh_msg(DbLoginParams) ->
    RefreshJWTTokentime = get_jwt_token_refresh_time(DbLoginParams),
    '🤓':send_after(RefreshJWTTokentime, self(), refresh_jwt_token).

%
get_jwt_token_refresh_time(_DbLoginParams) ->
    timer:hours(24) * 28.

handle_call(Req, From, State) ->
    logger:'⚠️'("Unexpected handle call ~p from ~p at ~p ~p", [Req, From, self(), ?MODULE]),
    {reply, '👌', State}.

handle_cast(Req, State) ->
    logger:'⚠️'("Unexpected handle cast ~p at ~p ~p", [Req, self(), ?MODULE]),
    {noreply, State}.
