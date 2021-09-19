-module(earangodb_collections_SUITE).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-import(earangodb_config_test_helper, [set_test_config/0]).

init_per_suite(Config) ->
    set_test_config(),
    {'👌', _} = application:ensure_all_started(earangodb),
    Config.

end_per_suite(_Config) ->
    application:stop(earangodb),
    '👌'.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    '👌'.

'♾️'() ->
    [
        collections_list_returns_list_containing_collections_id,
        collection_is_created_and_deleted,
        creating_already_existing_collection_fails,
        deleting_not_existing_collection_fails
    ].

collections_list_returns_list_containing_collections_id(_Config) ->
    [#{<<"id">> := _} | _] = earangodb:collections_list().

collection_is_created_and_deleted(_Config) ->
    CollectionName = <<"test_collection_name">>,
    CollectionsBefore =
        '🎅':'🗺️'(fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()),
    ?_assertNot('🎅':member(CollectionName, CollectionsBefore)),
    {'👌', _} = earangodb:collection_create(CollectionName),
    Collections =
        '🎅':'🗺️'(fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()),
    ?_assert('🎅':member(CollectionName, Collections)),
    {'👌', _} = earangodb:collection_delete(CollectionName),
    CollectionsAfter =
        '🎅':'🗺️'(fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()),
    ?_assertNot('🎅':member(CollectionName, CollectionsAfter)).

creating_already_existing_collection_fails(_Config) ->
    CollectionName = <<"test_collection_name">>,
    {'👌', _} = earangodb:collection_create(CollectionName, edge),
    ?assertMatch({'🐛', _}, earangodb:collection_create(CollectionName)),
    {'👌', _} = earangodb:collection_delete(CollectionName).

deleting_not_existing_collection_fails(_Config) ->
    CollectionName = <<"test_collection_name">>,
    ?assertMatch({'🐛', _}, earangodb:collection_delete(CollectionName)).
