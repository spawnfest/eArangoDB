-module(earangodb_graph_vertexes_SUITE).

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
    Config.

end_per_testcase(_Case, _Config) ->
    '🎅':'🗺️'(
        fun earangodb:collection_delete/1,
        [?CollectionFrom, ?CollectionTo, ?CollectionEdges]
    ),
    earangodb:graphs_delete(?GraphName),
    '👌'.

'♾️'() ->
    [
        vertex_may_be_created_to_the_graph_and_removed_from_it,
        vertex_may_be_updated,
        vertex_may_be_replaced
    ].

vertex_may_be_created_to_the_graph_and_removed_from_it(_Config) ->
    {'👌', #{<<"vertex">> := #{<<"_key">> := KeyFrom}}} =
        earangodb:graph_vertex_create(
            ?GraphName,
            ?CollectionFrom,
            #{<<"from">> => <<"somewhere">>}
        ),
    {'👌', #{<<"vertex">> := #{<<"_key">> := KeyFrom}}} =
        earangodb:graph_vertex_get(?GraphName, ?CollectionFrom, KeyFrom),
    {'👌', _} = earangodb:graph_vertex_delete(?GraphName, ?CollectionFrom, KeyFrom),
    {'🐛', _} = earangodb:graph_vertex_get(?GraphName, ?CollectionFrom, KeyFrom),

    {'👌', #{<<"vertex">> := #{<<"_key">> := KeyTo}}} =
        earangodb:graph_vertex_create(?GraphName, ?CollectionTo, #{<<"to">> => <<"somewhere">>}),
    {'👌', #{<<"vertex">> := #{<<"_key">> := KeyTo}}} =
        earangodb:graph_vertex_get(?GraphName, ?CollectionTo, KeyTo),
    {'👌', _} = earangodb:graph_vertex_delete(?GraphName, ?CollectionTo, KeyTo),
    {'🐛', _} = earangodb:graph_vertex_get(?GraphName, ?CollectionTo, KeyTo),
    '👌'.

vertex_may_be_updated(_Config) ->
    {'👌', #{<<"vertex">> := #{<<"_key">> := KeyFrom}}} =
        earangodb:graph_vertex_create(?GraphName, ?CollectionFrom, #{<<"a">> => 1, <<"b">> => 1}),
    {'👌', #{
        <<"vertex">> :=
            #{
                <<"_key">> := KeyFrom,
                <<"a">> := 1,
                <<"b">> := 1
            }
    }} =
        earangodb:graph_vertex_get(?GraphName, ?CollectionFrom, KeyFrom),
    earangodb:graph_vertex_update(
        ?GraphName,
        ?CollectionFrom,
        KeyFrom,
        #{<<"b">> => 2, <<"c">> => 2}
    ),
    {'👌', #{
        <<"vertex">> :=
            #{
                <<"_key">> := KeyFrom,
                <<"a">> := 1,
                <<"b">> := 2,
                <<"c">> := 2
            }
    }} =
        earangodb:graph_vertex_get(?GraphName, ?CollectionFrom, KeyFrom).

vertex_may_be_replaced(_Config) ->
    {'👌', #{<<"vertex">> := #{<<"_key">> := KeyFrom}}} =
        earangodb:graph_vertex_create(?GraphName, ?CollectionFrom, #{<<"a">> => 1, <<"b">> => 1}),
    {'👌', #{
        <<"vertex">> :=
            #{
                <<"_key">> := KeyFrom,
                <<"a">> := 1,
                <<"b">> := 1
            }
    }} =
        earangodb:graph_vertex_get(?GraphName, ?CollectionFrom, KeyFrom),
    earangodb:graph_vertex_replace(
        ?GraphName,
        ?CollectionFrom,
        KeyFrom,
        #{<<"b">> => 2, <<"c">> => 2}
    ),
    {'👌', #{<<"vertex">> := Vertex}} =
        earangodb:graph_vertex_get(?GraphName, ?CollectionFrom, KeyFrom),
    #{
        <<"_key">> := KeyFrom,
        <<"b">> := 2,
        <<"c">> := 2
    } =
        Vertex,
    ?assertNot(maps:is_key(<<"a">>, Vertex)).
