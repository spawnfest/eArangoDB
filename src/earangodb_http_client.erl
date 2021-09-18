-module(earangodb_http_client).

-export([get_token/4]).

get_token(User, Password, Url, Port) ->
    ReqBody = jiffy:encode(#{username => User, password => Password}),
    Headers = [],
    URIMap = #{scheme => "http", host => Url, port => Port, path => "/_open/auth"},
    FullUrl = uri_string:recompose(URIMap),
    Response = send_request_and_process_response(
        post, FullUrl, Headers, ReqBody
    ),
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
