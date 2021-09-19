demo_cities
=====

This is a demo project including earangodb library, building a simple graph (as shown below), and showing some simple operations that can be executed with the earangodb library.

The below graph is a slightly extended graph of cities taken from [here](https://www.arangodb.com/docs/3.8/graphs.html#the-city-graph)

![Cities graph](demo_graph.png?raw=true "Cities graph")

Start the database using `docker-compose up -d` and then a project using `rebar3 shell`, and then load the graph to the database by calling:

```erlang
demo_cities:setup_env().
```

Now lets try finding the shortest path between Paris and Warsaw on our graph:

```erlang
ShortestPathParisWarsawQuery = <<"FOR v, e IN OUTBOUND SHORTEST_PATH 'cities/Paris' TO 'cities/Warsaw' GRAPH 'cities_graph' RETURN [v.name, e.distance]">>,
    {ok, _} = earangodb:execute_query(ShortestPathParisWarsawQuery, true, 100).
```

You should see:

```erlang
{ok,[[<<"Paris">>,null],
     [<<"Berlin">>,1200],
     [<<"Warsaw">>,380]]}
```

as a result.

