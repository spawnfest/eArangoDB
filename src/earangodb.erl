%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @doc
%%% This module provides functions to work with collections, documents and graphs.
%%% It is a main API for this library.

-module(earangodb).

-type ok_response_or_error_reason() :: {ok, map()} | {error, Reason :: term()}.

-export([
    execute_query/3,
    collections_list/0,
    collection_create/1,
    collection_create/2,
    collection_delete/1,

    document_create/2,
    document_read/2,
    document_update/3,
    document_replace/3,
    document_delete/2,

    graphs_list/0,
    graphs_create/4,
    graphs_delete/1,
    graphs_get/1,

    graph_vertex_create/3,
    graph_vertex_delete/3,
    graph_vertex_get/3,
    graph_vertex_update/4,
    graph_vertex_replace/4,

    graph_edge_create/5,
    graph_edge_delete/3,
    graph_edge_get/3,
    graph_edge_update/4
]).

-ignore_xref([
    {?MODULE, execute_query, 3},
    {?MODULE, collections_list, 0},
    {?MODULE, collection_create, 1},
    {?MODULE, collection_create, 2},
    {?MODULE, collection_delete, 1},

    {?MODULE, document_create, 2},
    {?MODULE, document_read, 2},
    {?MODULE, document_update, 3},
    {?MODULE, document_replace, 3},
    {?MODULE, document_delete, 2},

    {?MODULE, graphs_list, 0},
    {?MODULE, graphs_create, 4},
    {?MODULE, graphs_delete, 1},
    {?MODULE, graphs_get, 1},

    {?MODULE, graph_vertex_create, 3},
    {?MODULE, graph_vertex_delete, 3},
    {?MODULE, graph_vertex_get, 3},
    {?MODULE, graph_vertex_update, 4},
    {?MODULE, graph_vertex_replace, 4},

    {?MODULE, graph_edge_create, 5},
    {?MODULE, graph_edge_delete, 3},
    {?MODULE, graph_edge_get, 3},
    {?MODULE, graph_edge_update, 4}
]).

%%% @doc
%%% Allows to execute AQL queries.
-spec execute_query(Query :: binary(), Count :: boolean(), BatchSize :: pos_integer()) ->
    {ok, Result :: [map()]} | {error, Reason :: term()}.
execute_query(Query, Count, BatchSize) ->
    Body = jiffy:encode(#{<<"query">> => Query, count => Count, batchSize => BatchSize}),
    case earangodb_http_client:send_request_and_unwrap_response(post, "/_api/cursor", Body) of
        Error = {error, _} ->
            Error;
        {ok, #{<<"hasMore">> := false, <<"result">> := Results}} ->
            {ok, Results};
        {ok, #{<<"hasMore">> := true, <<"result">> := Results, <<"id">> := CoursorId}} ->
            keep_on_fetching(Query, Count, BatchSize, CoursorId, Results)
    end.

keep_on_fetching(Query, Count, BatchSize, CoursorId, Results) ->
    Body = jiffy:encode(#{
        <<"query">> => Query, count => Count, batchSize => BatchSize
    }),
    Url = "/_api/cursor/" ++ earangodb_http_client:maybe_binary_to_lst(CoursorId),
    case earangodb_http_client:send_request_and_unwrap_response(post, Url, Body) of
        {ok, #{<<"hasMore">> := false, <<"result">> := NewResults}} ->
            {ok, NewResults ++ Results};
        {ok, #{<<"hasMore">> := true, <<"result">> := NewResults, <<"id">> := NewCoursorId}} ->
            keep_on_fetching(Query, Count, BatchSize, NewCoursorId, NewResults ++ Results)
    end.

%%% @doc
%%% Returns an object with an attribute result containing an array of all collection descriptions.
-spec collections_list() -> [earangodb_collections:collection_info()].
collections_list() -> earangodb_collections:list().

%%% @doc
%%% The same as `collection_create(Name, document)' function
-spec collection_create(Name :: binary()) -> ok_response_or_error_reason().
collection_create(Name) -> collection_create(Name, document).

%%% @doc
%%% Creates a new collection with a given name and type.
-spec collection_create(
    Name :: binary(), CollectionType :: earangodb_collections:collection_type()
) -> ok_response_or_error_reason().
collection_create(Name, Type) -> earangodb_collections:create(Name, Type).

%%% @doc
%%% Drops the collection identified by collection-name.
-spec collection_delete(Name :: binary()) -> ok_response_or_error_reason().
collection_delete(Name) -> earangodb_collections:delete(Name).

%%% @doc
%%% Creates a new document from the document given in the body, unless there is already a document with the _key given.
-spec document_create(CollectionName :: binary(), Document :: map()) ->
    ok_response_or_error_reason().
document_create(CollectionName, Document) -> earangodb_documents:create(CollectionName, Document).

%%% @doc
%%% Returns the document identified by document-id. The returned
%%% document contains three special attribute _key containing key which uniquely identifies a document in a given collection.
-spec document_read(CollectionName :: binary(), DocumentKey :: binary()) ->
    ok_response_or_error_reason().
document_read(CollectionName, DocumentKey) -> earangodb_documents:read(CollectionName, DocumentKey).

%%% @doc
%%% Partially updates the document identified by document-id.
%%% The body of the request must contain a JSON document with the
%%% attributes to patch (the patch document). All attributes from the
%%% patch document will be added to the existing document if they do not
%%% yet exist, and overwritten in the existing document if they do exist there.
-spec document_update(CollectionName :: binary(), DocumentKey :: binary(), Document :: map()) ->
    ok_response_or_error_reason().
document_update(CollectionName, DocumentKey, Document) ->
    earangodb_documents:update(CollectionName, DocumentKey, Document).

%%% @doc
%%% Replaces the specified document with the one in the body, provided there is such a document .
-spec document_replace(CollectionName :: binary(), DocumentKey :: binary(), Document :: map()) ->
    ok_response_or_error_reason().
document_replace(CollectionName, DocumentKey, Document) ->
    earangodb_documents:replace(CollectionName, DocumentKey, Document).

%%% @doc
%%% Deletes the specified document with a given key.
-spec document_delete(CollectionName :: binary(), DocumentKey :: binary()) ->
    ok_response_or_error_reason().
document_delete(CollectionName, DocumentKey) ->
    earangodb_documents:delete(CollectionName, DocumentKey).

%%% @doc
%%% Lists all graphs stored in this database.
-spec graphs_list() -> ok_response_or_error_reason().
graphs_list() -> earangodb_graphs:list().

%%% @doc
%%% The creation of a graph requires the name of the graph and a definition of its edges.
-spec graphs_create(
    GraphName :: binary(),
    EdgesCollection :: binary(),
    FromCollections :: [binary()],
    ToCollections :: [binary()]
) -> ok_response_or_error_reason().
graphs_create(GraphName, EdgesCollection, FromCollections, ToCollections) ->
    earangodb_graphs:create(GraphName, EdgesCollection, FromCollections, ToCollections).

%%% @doc
%%% Drops an existing graph object by name.
-spec graphs_delete(GraphName :: binary()) -> ok_response_or_error_reason().
graphs_delete(GraphName) -> earangodb_graphs:delete(GraphName).

%%% @doc
%%% Selects information for a given graph.
%%% Will return the edge definitions as well as the orphan collections.
%%% Or returns a 404 if the graph does not exist.
-spec graphs_get(GraphName :: binary()) -> ok_response_or_error_reason().
graphs_get(GraphName) -> earangodb_graphs:get(GraphName).

%%% @doc
%%% Adds a vertex to the given collection.
-spec graph_vertex_create(
    GraphName :: binary(), CollectionName :: binary(), VertexDocument :: map()
) -> ok_response_or_error_reason().
graph_vertex_create(GraphName, CollectionName, VertexDocument) ->
    earangodb_graph_vertexes:create(GraphName, CollectionName, VertexDocument).

%%% @doc
%%% Removes a vertex from the collection.
-spec graph_vertex_delete(GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary()) ->
    ok_response_or_error_reason().
graph_vertex_delete(GraphName, CollectionName, VertexKey) ->
    earangodb_graph_vertexes:delete(GraphName, CollectionName, VertexKey).

%%% @doc
%%% Gets a vertex from the collection with a given key.
-spec graph_vertex_get(GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary()) ->
    ok_response_or_error_reason().
graph_vertex_get(GraphName, CollectionName, VertexKey) ->
    earangodb_graph_vertexes:get(GraphName, CollectionName, VertexKey).

%%% @doc
%%% Updates the data of the specific vertex in the collection.
-spec graph_vertex_update(
    GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary(), UpdateDoc :: map()
) ->
    ok_response_or_error_reason().
graph_vertex_update(GraphName, CollectionName, VertexKey, UpdateDoc) ->
    earangodb_graph_vertexes:update(GraphName, CollectionName, VertexKey, UpdateDoc).

%%% @doc
%%% Replaces the data of the specific vertex in the collection.
-spec graph_vertex_replace(
    GraphName :: binary(), CollectionName :: binary(), VertexKey :: binary(), ReplaceDoc :: map()
) ->
    ok_response_or_error_reason().
graph_vertex_replace(GraphName, CollectionName, VertexKey, ReplaceDoc) ->
    earangodb_graph_vertexes:replace(GraphName, CollectionName, VertexKey, ReplaceDoc).

%%% @doc
%%% Creates a new edge in the collection.
-spec graph_edge_create(
    GraphName :: binary(),
    EdgesCollectionName :: binary(),
    FromVertex :: integer(),
    ToVertex :: integer(),
    Properties :: map()
) -> ok_response_or_error_reason().
graph_edge_create(GraphName, EdgesCollectionName, FromVertex, ToVertex, Properties) ->
    earangodb_graph_edges:create(GraphName, EdgesCollectionName, FromVertex, ToVertex, Properties).

%%% @doc
%%% Removes an edge from the collection.
-spec graph_edge_delete(GraphName :: binary(), CollectionName :: binary(), EdgeKey :: binary()) ->
    ok_response_or_error_reason().
graph_edge_delete(GraphName, CollectionName, EdgeKey) ->
    earangodb_graph_edges:delete(GraphName, CollectionName, EdgeKey).

%%% @doc
%%% Gets an edge from the given collection.
-spec graph_edge_get(GraphName :: binary(), CollectionName :: binary(), EdgeKey :: binary()) ->
    ok_response_or_error_reason().
graph_edge_get(GraphName, CollectionName, EdgeKey) ->
    earangodb_graph_edges:get(GraphName, CollectionName, EdgeKey).

%%% @doc
%%% Updates an edge from the given collection.
-spec graph_edge_update(
    GraphName :: binary(), CollectionName :: binary(), EdgeKey :: binary(), UpdateDoc :: map()
) ->
    ok_response_or_error_reason().
graph_edge_update(GraphName, CollectionName, EdgeKey, UpdateDoc) ->
    earangodb_graph_edges:update(GraphName, CollectionName, EdgeKey, UpdateDoc).
