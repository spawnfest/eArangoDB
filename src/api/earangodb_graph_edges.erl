-module(earangodb_graph_edges).

-export([
    create/5,
    delete/3,
    get/3,
    update/4
]).

-spec create(GraphName :: binary(), EdgeCollectionName  :: binary(), FromVertex :: integer(), ToVertex :: integer(), Properties :: map()) -> {ok, Result :: map()} | {error, Reason :: term()}.
create(GraphName, EdgeCollectionName, FromVertex, ToVertex, Properties) ->
    Url = "/_api/gharial/" ++ earangodb_http_client:maybe_binary_to_lst(GraphName)
        ++ "/edge/" ++ earangodb_http_client:maybe_binary_to_lst(EdgeCollectionName), 
    Body = jiffy:encode(Properties#{<<"_from">> => FromVertex, <<"_to">> => ToVertex}),
    earangodb_http_client:send_request_and_unwrap_response(post, Url, Body).

-spec delete(GraphName :: binary(), CollectionName  :: binary(), EdgeKey :: binary()) -> {ok, Result :: map()} | {error, Reason :: term()}.
delete(GraphName, CollectionName, EdgeKey) ->
    Url = url(GraphName, CollectionName, EdgeKey), 
    earangodb_http_client:send_request_and_unwrap_response(delete, Url, <<"">>).

-spec get(GraphName :: binary(), CollectionName  :: binary(), EdgeKey :: binary()) -> {ok, Result :: map()} | {error, Reason :: term()}.
get(GraphName, CollectionName, EdgeKey) ->
    Url = url(GraphName, CollectionName, EdgeKey), 
    earangodb_http_client:send_request_and_unwrap_response(get, Url, <<"">>).

-spec update(GraphName :: binary(), CollectionName  :: binary(), EdgeKey :: binary(), UpdateDoc :: map()) -> {ok, Result :: map()} | {error, Reason :: term()}.
update(GraphName, CollectionName, EdgeKey, UpdateDoc) ->
    Url = url(GraphName, CollectionName, EdgeKey), 
    earangodb_http_client:send_request_and_unwrap_response(patch, Url, jiffy:encode(UpdateDoc)).

url(GraphName, CollectionName, EdgeKey) ->
    "/_api/gharial/" ++ earangodb_http_client:maybe_binary_to_lst(GraphName)
        ++ "/edge/" ++ earangodb_http_client:maybe_binary_to_lst(CollectionName)
        ++ "/" ++ earangodb_http_client:maybe_binary_to_lst(EdgeKey).