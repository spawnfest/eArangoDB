-module(earangodb_colections).


-type collection_info() ::
    #{ Key :: binary() => Value :: binary() | boolean() | integer() }.

-export([
    list/0
]).

-spec list() -> [collection_info()].
list() ->
    UriMap = earangodb_http_client:build_url("/_api/collection"),
    Url = uri_string:recompose(UriMap),
    Headers = earangodb_http_client:build_headers(),
    Response = earangodb_http_client:send_request_and_process_response(get, Url, Headers, <<"">>),
    {ok, 200, #{<<"result">> := Result}} = Response,
    Result.
