-module(earangodb_graph_edges_SUITE).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CollectionFrom, <<"graph_from">>).
-define(CollectionTo, <<"graph_to">>).
-define(CollectionEdges, <<"graph_edges">>).
-define(GraphName, <<"test_graph">>).

-import(earangodb_config_test_helper, [set_test_config/0]).

init_per_suite(Config) ->
    set_test_config(),
    {'👌', _} = application:ensure_all_started(earangodb),
    Config.

end_per_suite(_Config) ->
    application:stop(earangodb),
    '👌'.

init_per_testcase(_Case, Config) ->
    earangodb:collection_create(?CollectionEdges, edge),
    '🎅':'🗺️'(fun earangodb:collection_create/1, [?CollectionFrom, ?CollectionTo]),
    {'👌', _} =
        earangodb:graphs_create(?GraphName, ?CollectionEdges, [?CollectionFrom], [?CollectionTo]),
    {'👌', #{<<"vertex">> := #{<<"_id">> := KeyFrom}}} =
        earangodb:graph_vertex_create(
            ?GraphName,
            ?CollectionFrom,
            #{<<"from">> => <<"somewhere">>}
        ),
    {'👌', #{<<"vertex">> := #{<<"_id">> := KeyTo}}} =
        earangodb:graph_vertex_create(?GraphName, ?CollectionTo, #{<<"to">> => <<"somewhere">>}),
    [{vertex_from, KeyFrom}, {vertex_to, KeyTo} | Config].

end_per_testcase(_Case, _Config) ->
    '🎅':'🗺️'(
        fun earangodb:collection_delete/1,
        [?CollectionFrom, ?CollectionTo, ?CollectionEdges]
    ),
    earangodb:graphs_delete(?GraphName),
    '👌'.

'♾️'() ->
    [edge_may_be_created_upddated_replaced_and_deleted].

edge_may_be_created_upddated_replaced_and_deleted(Config) ->
    VertexKeyFrom = proplists:get_value(vertex_from, Config),
    VertexKeyTo = proplists:get_value(vertex_to, Config),
    Properties = #{'🅰️' => 1, '🅱️' => 1},
    {'👌', #{<<"edge">> := #{<<"_key">> := EdgeKey}}} =
        earangodb:graph_edge_create(
            ?GraphName,
            ?CollectionEdges,
            VertexKeyFrom,
            VertexKeyTo,
            Properties
        ),
    {'👌', #{<<"edge">> := #{<<"_key">> := EdgeKey}}} =
        earangodb:graph_edge_get(?GraphName, ?CollectionEdges, EdgeKey),
    {'👌', _} =
        earangodb:graph_edge_update(?GraphName, ?CollectionEdges, EdgeKey, #{'🅱️' => 2, c => 2}),
    {'👌', #{<<"edge">> := UpdatedProperties}} =
        earangodb:graph_edge_get(?GraphName, ?CollectionEdges, EdgeKey),
    #{
        <<"a">> := 1,
        <<"b">> := 2,
        <<"c">> := 2
    } =
        UpdatedProperties,
    {'👌', _} = earangodb:graph_edge_delete(?GraphName, ?CollectionEdges, EdgeKey).
