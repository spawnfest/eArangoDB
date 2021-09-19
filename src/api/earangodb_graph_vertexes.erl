-module(earangodb_graph_vertexes).

-export([
    create/3,
    delete/3,
    get/3,
    update/4,
    replace/4
]).

-spec create(GraphName :: binary(), CollectionName :: binary(), VertexDocument :: map()) -> {ok, Result :: map()} | {error, Reason :: term()}.
create(GraphName, CollectionName, VertexDocument) ->
    Url = "/_api/gharial/" ++ earangodb_http_client:maybe_binary_to_lst(GraphName)
        ++ "/vertex/" ++ earangodb_http_client:maybe_binary_to_lst(CollectionName), 
    Body = jiffy:encode(VertexDocument),
    earangodb_http_client:send_request_and_unwrap_response(post, Url, Body).

-spec delete(GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary()) -> {ok, Result :: map()} | {error, Reason :: term()}.
delete(GraphName, CollectionName, VertexKey) ->
    Url = url(GraphName, CollectionName, VertexKey),
    earangodb_http_client:send_request_and_unwrap_response(delete, Url, <<"">>).

-spec get(GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary()) -> {ok, Result :: map()} | {error, Reason :: term()}.
get(GraphName, CollectionName, VertexKey) ->
    Url = url(GraphName, CollectionName, VertexKey),
    earangodb_http_client:send_request_and_unwrap_response(get, Url, <<"">>).

-spec update(GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary(), UpdateDocument :: map()) -> {ok, Result :: map()} | {error, Reason :: term()}.
update(GraphName, CollectionName, VertexKey, UpdateDocument) ->
    Url = url(GraphName, CollectionName, VertexKey),
    Body = jiffy:encode(UpdateDocument),
    earangodb_http_client:send_request_and_unwrap_response(patch, Url, Body).

-spec replace(GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary(), ReplaceDocument :: map()) -> {ok, Result :: map()} | {error, Reason :: term()}.
replace(GraphName, CollectionName, VertexKey, ReplaceDocument) ->
    Url = url(GraphName, CollectionName, VertexKey),
    Body = jiffy:encode(ReplaceDocument),
    earangodb_http_client:send_request_and_unwrap_response(put, Url, Body).

url(GraphName, CollectionName, VertexKey) ->
    "/_api/gharial/" ++ earangodb_http_client:maybe_binary_to_lst(GraphName)
        ++ "/vertex/" ++ earangodb_http_client:maybe_binary_to_lst(CollectionName)
        ++ "/" ++ earangodb_http_client:maybe_binary_to_lst(VertexKey).
