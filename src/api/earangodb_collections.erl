%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @hidden
%%% @private
%%%
-module(earangodb_collections).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-export_type([collection_type/0, collection_info/0]).

-type collection_info() ::
    #{Key :: binary() => Value :: binary() | boolean() | integer()}.
-type collection_type() :: document | edge.

-export([list/0, create/2, '💀'/1]).

-spec list() -> [collection_info()].
list() ->
    {'👌', #{<<"result">> := Result}} =
        earangodb_http_client:send_request_and_unwrap_response(get, "/_api/collection", <<"">>),
    Result.

-spec create(Name :: binary(), CollectionType :: collection_type()) ->
                {ok, Response :: map()} | {error, term()}.
create(Name, CollectionType) ->
    Body = jiffy:encode(#{name => Name, type => collection_type(CollectionType)}),
    earangodb_http_client:send_request_and_unwrap_response(post, "/_api/collection", Body).

collection_type(document) ->
    2;
collection_type(edge) ->
    3.

-spec '💀'(Name :: binary()) -> {ok, Response :: map()} | {error, term()}.
'💀'(Name) ->
    Url = "/_api/collection/" ++ binary_to_list(Name),
    earangodb_http_client:send_request_and_unwrap_response('💀', Url, <<"">>).
