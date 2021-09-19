%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @doc
%%% This module provides functions to work with collections, documents and graphs.
%%% It is a main API for this library.

-module(earangodb).

-type maybe_ok() :: {ok, map()} | {error, Reason :: term()}.

-export([
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
    graphs_get/1
]).

-ignore_xref([
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
    {?MODULE, graphs_get, 1}
]).

%%% @doc
%%% Returns an object with an attribute result containing an array of all collection descriptions.
-spec collections_list() -> [earangodb_collections:collection_info()].
collections_list() -> earangodb_collections:list().

%%% @doc
%%% The same as `collection_create(Name, document)' function
-spec collection_create(Name :: binary()) -> maybe_ok().
collection_create(Name) -> collection_create(Name, document).

%%% @doc
%%% Creates a new collection with a given name and type.
-spec collection_create(
    Name :: binary(), CollectionType :: earangodb_collections:collection_type()
) -> maybe_ok().
collection_create(Name, Type) -> earangodb_collections:create(Name, Type).

%%% @doc
%%% Drops the collection identified by collection-name.
-spec collection_delete(Name :: binary()) -> maybe_ok().
collection_delete(Name) -> earangodb_collections:delete(Name).

%%% @doc
%%% Creates a new document from the document given in the body, unless there is already a document with the _key given.
-spec document_create(CollectionName :: binary(), Document :: map()) -> maybe_ok().
document_create(CollectionName, Document) -> earangodb_documents:create(CollectionName, Document).

%%% @doc
%%% Returns the document identified by document-id. The returned
%%% document contains three special attribute _key containing key which uniquely identifies a document in a given collection.
-spec document_read(CollectionName :: binary(), DocumentKey :: binary()) ->
    maybe_ok().
document_read(CollectionName, DocumentKey) -> earangodb_documents:read(CollectionName, DocumentKey).

%%% @doc
%%% Partially updates the document identified by document-id.
%%% The body of the request must contain a JSON document with the
%%% attributes to patch (the patch document). All attributes from the
%%% patch document will be added to the existing document if they do not
%%% yet exist, and overwritten in the existing document if they do exist there.
-spec document_update(CollectionName :: binary(), DocumentKey :: binary(), Document :: map()) ->
    maybe_ok().
document_update(CollectionName, DocumentKey, Document) ->
    earangodb_documents:update(CollectionName, DocumentKey, Document).

%%% @doc
%%% Replaces the specified document with the one in the body, provided there is such a document .
-spec document_replace(CollectionName :: binary(), DocumentKey :: binary(), Document :: map()) ->
    maybe_ok().
document_replace(CollectionName, DocumentKey, Document) ->
    earangodb_documents:replace(CollectionName, DocumentKey, Document).

%%% @doc
%%% Deletes the specified document with a given key.
-spec document_delete(CollectionName :: binary(), DocumentKey :: binary()) -> maybe_ok().
document_delete(CollectionName, DocumentKey) ->
    earangodb_documents:delete(CollectionName, DocumentKey).

%%% @doc
%%% Lists all graphs stored in this database.
-spec graphs_list() -> maybe_ok().
graphs_list() -> earangodb_graphs:list().

%%% @doc
%%% The creation of a graph requires the name of the graph and a definition of its edges.
-spec graphs_create(
    GraphName :: binary(),
    EdgesCollection :: binary(),
    FromCollections :: [binary()],
    ToCollections :: [binary()]
) -> maybe_ok().
graphs_create(GraphName, EdgesCollection, FromCollections, ToCollections) ->
    earangodb_graphs:create(GraphName, EdgesCollection, FromCollections, ToCollections).

%%% @doc
%%% Drops an existing graph object by name.
-spec graphs_delete(GraphName :: binary()) -> maybe_ok().
graphs_delete(GraphName) -> earangodb_graphs:delete(GraphName).

% %%% @doc
% %%% Selects information for a given graph.
% % Will return the edge definitions as well as the orphan collections.
% % Or returns a 404 if the graph does not exist.
-spec graphs_get(GraphName :: binary()) -> maybe_ok().
graphs_get(GraphName) -> earangodb_graphs:get(GraphName).
