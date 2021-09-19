-module(earangodb_documents_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CollectionName, <<"mycolecton">>).

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
    {ok, _} = earangodb:collection_create(?CollectionName),
    Collections = lists:map(
        fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()
    ),
    ?assert(lists:member(?CollectionName, Collections)),
    Config.

end_per_testcase(_Case, _Config) ->
    {ok, _} = earangodb_collections:delete(?CollectionName),
    Collections = lists:map(
        fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()
    ),
    ?assertNot(lists:member(?CollectionName, Collections)),
    ok.

all() ->
    [
        add_some_documents_to_collection,
        creat_document_in_not_exisitng_colection,
        create_document_with_a_given_key_twice,
        document_with_a_given_key_can_be_read,
        not_existing_document_cannot_be_read,
        replacing_existing_document_changes_it_completly,
        updating_existing_document_changes_only_updated_keys,
        when_deleted_document_cannot_be_read
    ].

add_some_documents_to_collection(_Config) ->
    Document = #{<<"hello">> => <<"word">>},
    % AfterDecodeColection = jiffy:decode(jiffy:encode(Collection)),
    {ok, _} = earangodb:document_create(?CollectionName, Document),
    ok.

creat_document_in_not_exisitng_colection(_Config) ->
    {error, _} = earangodb:document_create("not_existing_collection", #{}).

create_document_with_a_given_key_twice(_Config) ->
    Document = #{'_key' => <<"word">>},
    {ok, _} = earangodb:document_create(?CollectionName, Document),
    {error, _} = earangodb:document_create(?CollectionName, Document).

document_with_a_given_key_can_be_read(_Config) ->
    DocumentKey = <<"word_key">>,
    Document = #{'_key' => DocumentKey},
    {ok, _} = earangodb:document_create(?CollectionName, Document),
    {ok, #{<<"_key">> := <<"word_key">>}} = earangodb:document_read(?CollectionName, DocumentKey).

not_existing_document_cannot_be_read(_Config) ->
    DocumentKey = <<"not_existing_key">>,
    {error, _} = earangodb:document_read(?CollectionName, DocumentKey).

replacing_existing_document_changes_it_completly(_Config) ->
    DocumentKey = <<"asdf">>,
    OldDocument = #{'_key' => DocumentKey, a => 1, b => 2},
    NewDocument = #{'_key' => DocumentKey, c => 1, d => 2},
    {ok, _} = earangodb:document_create(?CollectionName, OldDocument),
    {ok, _} = earangodb:document_replace(?CollectionName, DocumentKey, NewDocument),
    {ok, ResultDocument} = earangodb:document_read(?CollectionName, DocumentKey),
    ?assert(maps:is_key(<<"c">>, ResultDocument)),
    ?assert(maps:is_key(<<"d">>, ResultDocument)),
    ?assertNot(maps:is_key(<<"a">>, ResultDocument)),
    ?assertNot(maps:is_key(<<"b">>, ResultDocument)),
    ok.

updating_existing_document_changes_only_updated_keys(_Config) ->
    DocumentKey = <<"asdf">>,
    OldDocument = #{'_key' => DocumentKey, a => 1, b => 1},
    NewDocument = #{'_key' => DocumentKey, b => 2, c => 2},
    {ok, _} = earangodb:document_create(?CollectionName, OldDocument),
    {ok, _} = earangodb:document_update(?CollectionName, DocumentKey, NewDocument),
    {ok, #{<<"a">> := 1, <<"b">> := 2, <<"c">> := 2}} = earangodb:document_read(
        ?CollectionName, DocumentKey
    ),
    ok.

when_deleted_document_cannot_be_read(_Config) ->
    DocumentKey = <<"asdf">>,
    OldDocument = #{'_key' => DocumentKey, a => 1, b => 1},
    {ok, _} = earangodb:document_create(?CollectionName, OldDocument),
    {ok, _} = earangodb:document_read(?CollectionName, DocumentKey),
    {ok, _} = earangodb:document_delete(?CollectionName, DocumentKey),
    {error, _} = earangodb:document_read(?CollectionName, DocumentKey).
