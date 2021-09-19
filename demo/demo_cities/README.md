demo_cities
=====

This is a demo project including earangodb library, building a simple graph (as shown below), and showing some simple operations that can be executed with the earangodb library.

The below graph is a slightly extended graph of cities taken from [here](https://www.arangodb.com/docs/3.8/graphs.html#the-city-graph)

![Cities graph](demo_graph.png?raw=true "Cities graph")

Start the database using `docker-compose up -d` and then a project using `rebar3 shell`, and then load the graph to the database by calling:

```erlang
demo_cities:setup_env().
```
Any query using AQL can be executed.

Let's try finding the shortest path between Paris and Warsaw on our graph:

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


We can eg see the names of the cities in the distance of of up to 2 jumps from Madrit:

```erlang
{ok, _} = earangodb:execute_query(<<"FOR v, e IN 1..1 OUTBOUND 'cities/Madrit' GRAPH 'cities_graph' RETURN v.name">>, true, 100).
```

the result should be:

```erlang
{ok,[<<"Paris">>,<<"Lyon">>]}
```

We can see how teh result chnages if we travel outbound instead of inbound, and use Berlin as an example city:

```erlang
{ok, _} = earangodb:execute_query(<<"FOR v, e IN 1..1 INBOUND 'cities/Berlin' GRAPH 'cities_graph' RETURN v.name">>, true, 100).
```

the result should be:

```erlang
{ok,[<<"Paris">>,<<"Cologne">>,<<"Hamburg">>,<<"Lyon">>,
     <<"Warsaw">>]}
```