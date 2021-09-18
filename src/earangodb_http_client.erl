-module(earangodb_http_client).

-export([
    get_token/2,
    send_request_and_process_response/4,
    build_headers/0,
    build_url/1
]).

get_token(User, Password) ->
    ReqBody = jiffy:encode(#{username => User, password => Password}),
    URIMap = build_url("/_open/auth"),
    FullUrl = uri_string:recompose(URIMap),
    Response = send_request_and_process_response(post, FullUrl, [], ReqBody),
    case Response of
        {ok, 200, #{<<"jwt">> := JWTToken}} ->
            {ok, JWTToken};
        {ok, ErrorCode, #{<<"errorMessage">> := ErrorMessage}} ->
            logger:warning("Token Bearer failed to update JWT token ~p ~p ", [
                ErrorCode, ErrorMessage
            ]),
            error(failed_to_update_token)
    end.

send_request_and_process_response(Method, Url, Headers, ReqBody) ->
    case hackney:request(Method, Url, Headers, ReqBody, []) of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            DecodedBody = jiffy:decode(ResponseBody, [return_maps]),
            {ok, StatusCode, DecodedBody};
        Response = {error, _Reason} ->
            Response
    end.

build_headers() ->
    Basic = <<"bearer ">>,
    Val = earangodb_token_bearer:get_jwt_token(),
    [
        {<<"Authorization">>, <<Basic/binary, Val/binary>>},
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json">>}
    ].

build_url(Path) ->
    BaseUri = persistent_term:get(earangodb_conn_uri_map),
    BaseUri#{path => Path}.
