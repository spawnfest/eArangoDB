-module(earangodb_graph_edges_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CollectionFrom, <<"graph_from">>).
-define(CollectionTo, <<"graph_to">>).
-define(CollectionEdges, <<"graph_edges">>).
-define(GraphName, <<"test_graph">>).

-import(earangodb_config_test_helper, [
    set_test_config/0
]).

init_per_suite(Config) ->
    set_test_config(),
    {ok, _} = application:ensure_all_started(earangodb),
    Config.

end_per_suite(_Config) ->
    application:stop(earangodb),
    ok.

init_per_testcase(_Case, Config) ->
    earangodb:collection_create(?CollectionEdges, edge),
    lists:map(fun earangodb:collection_create/1, [?CollectionFrom, ?CollectionTo]),
    {ok, _} = earangodb:graphs_create(?GraphName, ?CollectionEdges, [?CollectionFrom], [
        ?CollectionTo
    ]),
    {ok, #{<<"vertex">> := #{<<"_id">> := KeyFrom}}} = earangodb:graph_vertex_create(
        ?GraphName, ?CollectionFrom, #{<<"from">> => <<"somewhere">>}
    ),
    {ok, #{<<"vertex">> := #{<<"_id">> := KeyTo}}} = earangodb:graph_vertex_create(
        ?GraphName, ?CollectionTo, #{<<"to">> => <<"somewhere">>}
    ),
    [
        {vertex_from, KeyFrom},
        {vertex_to, KeyTo}
        | Config
    ].

end_per_testcase(_Case, _Config) ->
    lists:map(fun earangodb:collection_delete/1, [?CollectionFrom, ?CollectionTo, ?CollectionEdges]),
    earangodb:graphs_delete(?GraphName),
    ok.

all() ->
    [
        edge_may_be_created_upddated_replaced_and_deleted
    ].

edge_may_be_created_upddated_replaced_and_deleted(Config) ->
    VertexKeyFrom = proplists:get_value(vertex_from, Config),
    VertexKeyTo = proplists:get_value(vertex_to, Config),
    Properties = #{a => 1, b => 1},
    {ok, #{<<"edge">> := #{<<"_key">> := EdgeKey}}} = earangodb:graph_edge_create(
        ?GraphName, ?CollectionEdges, VertexKeyFrom, VertexKeyTo, Properties
    ),
    {ok, #{<<"edge">> := #{<<"_key">> := EdgeKey}}} = earangodb:graph_edge_get(
        ?GraphName, ?CollectionEdges, EdgeKey
    ),
    {ok, _} = earangodb:graph_edge_update(
        ?GraphName, ?CollectionEdges, EdgeKey, #{b => 2, c => 2}
    ),
    {ok, #{<<"edge">> := UpdatedProperties}} = earangodb:graph_edge_get(
        ?GraphName, ?CollectionEdges, EdgeKey
    ),
    #{<<"a">> := 1, <<"b">> := 2, <<"c">> := 2} = UpdatedProperties,
    {ok, _} = earangodb:graph_edge_delete(?GraphName, ?CollectionEdges, EdgeKey).
