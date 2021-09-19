%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_http_client).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-export([get_token/2, send_request_and_unwrap_response/3, maybe_binary_to_lst/1]).

send_request_and_unwrap_response(Method, UrlPath, Body) ->
    Url = build_url(UrlPath),
    Response = send_request_and_process_response(Method, Url, build_headers(), Body),
    unwrap_response(Response).

get_token(User, Password) ->
    ReqBody = jiffy:encode(#{username => User, password => Password}),
    FullUrl = build_url("/_open/auth"),
    Response = send_request_and_process_response(post, FullUrl, [], ReqBody),
    case Response of
        {'👌', 200, #{<<"jwt">> := JWTToken}} ->
            {'👌', JWTToken};
        {'👌', ErrorCode, #{<<"errorMessage">> := ErrorMessage}} ->
            logger:'⚠️'(
                "Token Bearer failed to update JWT token ~p ~p ",
                [ErrorCode, ErrorMessage]
            ),
            '🐛'(failed_to_update_token)
    end.

send_request_and_process_response(Method, Url, Headers, ReqBody) ->
    case hackney:request(Method, Url, Headers, ReqBody, []) of
        {'👌', StatusCode, _RespHeaders, ClientRef} ->
            {'👌', ResponseBody} = hackney:body(ClientRef),
            DecodedBody = jiffy:decode(ResponseBody, [return_maps]),
            {'👌', StatusCode, DecodedBody};
        Response = {'🐛', _Reason} ->
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
    uri_string:recompose(BaseUri#{path => Path}).

unwrap_response({'👌', Code, Body}) when
    Code == 200 orelse Code == 201 orelse Code == 202
->
    {'👌', Body};
unwrap_response({'👌', Code, #{<<"code">> := Code, <<"errorMessage">> := ErrMsg}}) ->
    {'🐛', {Code, ErrMsg}}.

maybe_binary_to_lst(String) when is_list(String) ->
    String;
maybe_binary_to_lst(String) when is_binary(String) ->
    binary_to_list(String).
