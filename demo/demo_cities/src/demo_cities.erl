-module(demo_cities).

-export([setup_env/0]).

setup_env() ->
    GraphName = <<"cities_graph">>,
    VertexesCollection = <<"cities">>,
    EdgesCollection = <<"cities_connections">>,

    {ok, _} = earangodb:collection_create(VertexesCollection),
    {ok, _} = earangodb:collection_create(EdgesCollection, edge),
    {ok, _} = earangodb:graphs_create(GraphName, EdgesCollection, [VertexesCollection], [VertexesCollection]),
    
    {ok, #{<<"vertex">> := #{<<"_id">> := ParisVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Paris">>, name => <<"Paris">>}),
    {ok, #{<<"vertex">> := #{<<"_id">> := LyonVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Lyon">>, name => <<"Lyon">>}),
    {ok, #{<<"vertex">> := #{<<"_id">> := BerlinVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Berlin">>, name => <<"Berlin">>}),
    {ok, #{<<"vertex">> := #{<<"_id">> := CologneVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Cologne">>, name => <<"Cologne">>}),
    {ok, #{<<"vertex">> := #{<<"_id">> := HamburgVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Hamburg">>, name => <<"Hamburg">>}),
    {ok, #{<<"vertex">> := #{<<"_id">> := MadritVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Madrit">>, name => <<"Madrit">>}),
    {ok, #{<<"vertex">> := #{<<"_id">> := WarsawVertexId}}} = earangodb:graph_vertex_create(GraphName, VertexesCollection, #{<<"_key">> => <<"Warsaw">>, name => <<"Warsaw">>}),

    Connections = [
        {ParisVertexId, LyonVertexId, 550},
        {ParisVertexId, HamburgVertexId, 900},
        {ParisVertexId, BerlinVertexId, 1200},
        {ParisVertexId, CologneVertexId, 550},
        {ParisVertexId, MadritVertexId, 550},

        {BerlinVertexId, CologneVertexId, 850},
        {BerlinVertexId, HamburgVertexId, 400},
        {BerlinVertexId, LyonVertexId, 1100},
        {BerlinVertexId, WarsawVertexId, 380},

        {CologneVertexId, LyonVertexId, 700},
        {CologneVertexId, HamburgVertexId, 500},
        {CologneVertexId, WarsawVertexId, 450},

        {LyonVertexId, HamburgVertexId, 1300},
        {LyonVertexId, MadritVertexId, 500}


    ],
    lists:map(fun({City1, City2, Distance}) ->
        earangodb:graph_edge_create(GraphName, EdgesCollection, City1, City2, #{distance => Distance}),
        earangodb:graph_edge_create(GraphName, EdgesCollection, City2, City1, #{distance => Distance})
    end, Connections),
    ok.

