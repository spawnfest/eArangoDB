-module(earangodb_graphs).

-export([
    list/0,
    create/4,
    delete/1,
    get/1
]).

-spec list() -> {ok, Graphs :: map()} | {error, Reason :: term()}.
list() ->
    earangodb_http_client:send_request_and_unwrap_response(get, "/_api/gharial/", <<"">>).
    

-spec create(GraphName :: binary(), EdgesCollection :: binary(), FromCollections :: [binary()], ToCollections :: [binary()]) -> {ok, Result :: map()} | {error, Reason :: term()}.
create(GraphName, EdgesCollection, FromCollections, ToCollections) ->
    Body = #{  <<"name">> => GraphName, 
        <<"edgeDefinitions">> => [ 
          #{ 
            <<"collection">> => EdgesCollection, 
            <<"from">> => FromCollections, 
            <<"to">> => ToCollections
          } 
        ] 
      },
    earangodb_http_client:send_request_and_unwrap_response(post, "/_api/gharial/", jiffy:encode(Body)).

-spec delete(GraphName :: binary()) -> {ok, Result :: map()} | {error, Reason :: term()}.
delete(GraphName) ->
    Url = "/_api/gharial/" ++ earangodb_http_client:maybe_binary_to_lst(GraphName),
    earangodb_http_client:send_request_and_unwrap_response(delete, Url, <<"">>).

get(GraphName) ->
  Url = "/_api/gharial/" ++ earangodb_http_client:maybe_binary_to_lst(GraphName),
    earangodb_http_client:send_request_and_unwrap_response(get, Url, <<"">>).
