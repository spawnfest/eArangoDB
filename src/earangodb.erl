%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @doc
%%% This module provides functions to work with collections, graphs
%%% It is a main API for this library.

-module(earangodb).

-export([
    collections_list/0
]).

-ignore_xref([
    {?MODULE, collections_list, 0}
]).

%%% @doc
%%% Returns an object with an attribute result containing an array of all collection descriptions.
%%% '''
collections_list() -> earangodb_colections:list().
