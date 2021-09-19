%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_collections).

-export_type([
    collection_type/0,
    collection_info/0
]).

-type collection_info() ::
    #{ Key :: binary() => Value :: binary() | boolean() | integer() }.

-type collection_type() :: document | edge.

-export([
    list/0,
    create/2,
    delete/1
]).

-spec list() -> [collection_info()].
list() ->
    Url = earangodb_http_client:build_url("/_api/collection"),
    Headers = earangodb_http_client:build_headers(),
    Response = earangodb_http_client:send_request_and_process_response(get, Url, Headers, <<"">>),
    {ok, 200, #{<<"result">> := Result}} = Response,
    Result.

-spec create(Name :: binary(), CollectionType :: collection_type()) -> {ok, Response :: map()} | {error, term()}.
create(Name, CollectionType) ->
    Url = earangodb_http_client:build_url("/_api/collection"),
    Headers = earangodb_http_client:build_headers(),
    Body = jiffy:encode(#{name => Name, type => collection_type(CollectionType)}),
    Response = earangodb_http_client:send_request_and_process_response(post, Url, Headers, Body),
    earangodb_http_client:unwrap_response(Response).

collection_type(document) -> 2;
collection_type(edge) -> 3.

-spec delete(Name :: binary()) -> {ok, Response :: map()} | {error, term()}.
delete(Name) ->
    Url = earangodb_http_client:build_url("/_api/collection/" ++ binary_to_list(Name)),
    Headers = earangodb_http_client:build_headers(),
    Response = earangodb_http_client:send_request_and_process_response(delete, Url, Headers, <<"">>),
    earangodb_http_client:unwrap_response(Response).
