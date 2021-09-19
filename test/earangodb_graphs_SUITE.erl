-module(earangodb_graphs_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CollectionFrom, <<"graph_from">>).
-define(CollectionTo, <<"graph_to">>).
-define(CollectionEdges, <<"graph_edges">>).

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
    Config.

end_per_testcase(_Case, _Config) ->
    lists:map(fun earangodb:collection_delete/1, [?CollectionFrom, ?CollectionTo, ?CollectionEdges]),
    ok.

all() ->
    [
        listing_graphs_returns_list,
        when_graph_is_created_it_is_listed_and_can_be_deleted,
        when_graph_is_created_it_can_be_accessed,
        when_graph_does_not_exist_it_cannot_be_accessed
    ].

listing_graphs_returns_list(_Config) ->
    {ok, #{<<"graphs">> := Graphs}} = earangodb:graphs_list(),
    ?assert(is_list(Graphs)).

when_graph_is_created_it_is_listed_and_can_be_deleted(_Config) ->
    GraphName = <<"test_graph">>,
    ?assertNot(graph_exists(GraphName)),
    {ok, _} = earangodb:graphs_create(GraphName, ?CollectionEdges, [?CollectionFrom], [
        ?CollectionTo
    ]),
    ?assert(graph_exists(GraphName)),
    {ok, _} = earangodb:graphs_delete(GraphName),
    ?assertNot(graph_exists(GraphName)).

graph_exists(GraphName) ->
    {ok, #{<<"graphs">> := GraphsBefore}} = earangodb:graphs_list(),
    GraphsNames = lists:map(fun(#{<<"name">> := Name}) -> Name end, GraphsBefore),
    lists:member(GraphName, GraphsNames).

when_graph_is_created_it_can_be_accessed(_Config) ->
    GraphName = <<"test_graph">>,
    {ok, _} = earangodb:graphs_create(GraphName, ?CollectionEdges, [?CollectionFrom], [
        ?CollectionTo
    ]),
    {ok, #{<<"graph">> := #{<<"name">> := GraphName}}} = earangodb_graphs:get(GraphName),
    {ok, _} = earangodb:graphs_delete(GraphName).

when_graph_does_not_exist_it_cannot_be_accessed(_Config) ->
    GraphName = <<"test_graph">>,
    ?assertNot(graph_exists(GraphName)),
    {error, _} = earangodb:graphs_get(GraphName),
    ok.
