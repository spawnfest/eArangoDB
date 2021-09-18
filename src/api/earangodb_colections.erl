%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_colections).

-export_type([collection_info/0]).
-type collection_info() ::
    #{ Key :: binary() => Value :: binary() | boolean() | integer() }.

-export([
    list/0,
    create/1,
    delete/1
]).

-spec list() -> [collection_info()].
list() ->
    Url = earangodb_http_client:build_url("/_api/collection"),
    Headers = earangodb_http_client:build_headers(),
    Response = earangodb_http_client:send_request_and_process_response(get, Url, Headers, <<"">>),
    {ok, 200, #{<<"result">> := Result}} = Response,
    Result.

-spec create(Name :: binary()) -> ok | {error, Reason :: term()}.
create(Name) ->
    Url = earangodb_http_client:build_url("/_api/collection"),
    Headers = earangodb_http_client:build_headers(),
    Body = jiffy:encode(#{name => Name}),
    Response = earangodb_http_client:send_request_and_process_response(post, Url, Headers, Body),
    case Response of
        {ok, 200, _} -> ok;
        Error -> {error, Error}
    end.

-spec delete(Name :: binary()) -> ok | {error, Reason :: term()}.
delete(Name) ->
    Url = earangodb_http_client:build_url("/_api/collection/" ++ binary_to_list(Name)),
    Headers = earangodb_http_client:build_headers(),
    Response = earangodb_http_client:send_request_and_process_response(delete, Url, Headers, <<"">>),
    case Response of
        {ok, 200, _} -> ok;
        Error -> {error, Error}
    end.
