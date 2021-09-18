-module(earangodb_colections_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

init_per_suite(Config) ->
    set_test_config(),
    {ok, _} = application:ensure_all_started(earangodb),
    Config.

end_per_suite(_Config) ->
    application:stop(earangodb),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) -> ok.

all() ->
    [
        colections_list_returns_list_containing_colections_id,
        collection_is_created_and_deleted,
        creating_already_existing_collection_fails,
        deleting_not_existing_collection_fails
    ].

colections_list_returns_list_containing_colections_id(_Config) ->
    [#{<<"id">> := _} | _] = earangodb:collections_list().

collection_is_created_and_deleted(_Config) ->
    CollectionName = <<"test_collection_name">>,
    CollectionsBefore = lists:map(
        fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()
    ),
    ?_assertNot(lists:member(CollectionName, CollectionsBefore)),
    ok = earangodb:collection_create(CollectionName),
    Collections = lists:map(fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()),
    ?_assert(lists:member(CollectionName, Collections)),
    ok = earangodb:collection_delete(CollectionName),
    CollectionsAfter = lists:map(
        fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()
    ),
    ?_assertNot(lists:member(CollectionName, CollectionsAfter)).

creating_already_existing_collection_fails(_Config) ->
    CollectionName = <<"test_collection_name">>,
    ok = earangodb:collection_create(CollectionName),
    ?assertMatch({error, _}, earangodb:collection_create(CollectionName)),
    ok = earangodb:collection_delete(CollectionName).

deleting_not_existing_collection_fails(_Config) ->
    CollectionName = <<"test_collection_name">>,
    ?assertMatch({error, _}, earangodb:collection_delete(CollectionName)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_test_config() ->
    Config = #{
        user => <<"root">>,
        password => <<"test">>,
        url => "localhost",
        port => 8529
    },
    persistent_term:put(earangodb_config, Config).
