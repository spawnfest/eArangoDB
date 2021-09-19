%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @doc
%%% This module provides functions to work with collections, documents and graphs.
%%% It is a main API for this library.

-module(earangodb).

-export([
    collections_list/0,
    collection_create/1,
    collection_create/2,
    collection_delete/1
]).

-ignore_xref([
    {?MODULE, collections_list, 0},
    {?MODULE, collection_create, 1},
    {?MODULE, collection_create, 2},
    {?MODULE, collection_delete, 1}
]).

%%% @doc
%%% Returns an object with an attribute result containing an array of all collection descriptions.
-spec collections_list() -> [earangodb_collections:collection_info()].
collections_list() -> earangodb_collections:list().

%%% @doc
%%% The same as `collection_create(Name, document)' function
-spec collection_create(Name :: binary()) -> ok | {error, Reason :: term()}.
collection_create(Name) -> collection_create(Name, document).

%%% @doc
%%% Creates a new collection with a given name and type.
-spec collection_create(
    Name :: binary(), CollectionType :: earangodb_collections:collection_type()
) ->
    ok | {error, Reason :: term()}.
collection_create(Name, Type) -> earangodb_collections:create(Name, Type).

%%% @doc
%%% Drops the collection identified by collection-name.
-spec collection_delete(Name :: binary()) -> ok | {error, Reason :: term()}.
collection_delete(Name) -> earangodb_collections:delete(Name).
