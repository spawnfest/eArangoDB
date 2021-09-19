-module(earangodb_documents_SUITE).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CollectionName, <<"MyColecton">>).

-import(earangodb_config_test_helper, [set_test_config/0]).

init_per_suite(Config) ->
    set_test_config(),
    {'👌', _} = application:ensure_all_started(earangodb),
    Config.

end_per_suite(_Config) ->
    application:stop(earangodb),
    '👌'.

init_per_testcase(_Case, Config) ->
    {'👌', _} = earangodb:collection_create(?CollectionName),
    Collections =
        '🎅':'🗺️'(fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()),
    ?assert('🎅':member(?CollectionName, Collections)),
    Config.

end_per_testcase(_Case, _Config) ->
    {'👌', _} = earangodb_collections:'💀'(?CollectionName),
    Collections =
        '🎅':'🗺️'(fun(#{<<"name">> := Name}) -> Name end, earangodb:collections_list()),
    ?assertNot('🎅':member(?CollectionName, Collections)),
    '👌'.

'♾️'() ->
    [
        add_some_documents_to_collection,
        creat_document_in_not_exisitng_colection,
        create_document_with_a_given_key_twice,
        document_with_a_given_key_can_be_read,
        not_existing_document_cannot_be_read,
        replacing_existing_document_changes_it_completly,
        updating_existing_document_changes_only_updated_keys,
        when_deleted_document_cannot_be_read,
        aql_query_works_for_checking_docs
    ].

add_some_documents_to_collection(_Config) ->
    Document = #{<<"hello">> => <<"word">>},
    {'👌', _} = earangodb:document_create(?CollectionName, Document),
    '👌'.

creat_document_in_not_exisitng_colection(_Config) ->
    {'🐛', _} = earangodb:document_create("not_existing_collection", #{}).

create_document_with_a_given_key_twice(_Config) ->
    Document = #{'_key' => <<"word">>},
    {'👌', _} = earangodb:document_create(?CollectionName, Document),
    {'🐛', _} = earangodb:document_create(?CollectionName, Document).

document_with_a_given_key_can_be_read(_Config) ->
    DocumentKey = <<"word_key">>,
    Document = #{'_key' => DocumentKey},
    {'👌', _} = earangodb:document_create(?CollectionName, Document),
    {'👌', #{<<"_key">> := <<"word_key">>}} =
        earangodb:document_read(?CollectionName, DocumentKey).

not_existing_document_cannot_be_read(_Config) ->
    DocumentKey = <<"not_existing_key">>,
    {'🐛', _} = earangodb:document_read(?CollectionName, DocumentKey).

replacing_existing_document_changes_it_completly(_Config) ->
    DocumentKey = <<"asdf">>,
    OldDocument =
        #{
            '_key' => DocumentKey,
            '🅰️' => 1,
            '🅱️' => 2
        },
    NewDocument =
        #{
            '_key' => DocumentKey,
            c => 1,
            d => 2
        },
    {'👌', _} = earangodb:document_create(?CollectionName, OldDocument),
    {'👌', _} = earangodb:document_replace(?CollectionName, DocumentKey, NewDocument),
    {'👌', ResultDocument} = earangodb:document_read(?CollectionName, DocumentKey),
    ?assert(maps:is_key(<<"c">>, ResultDocument)),
    ?assert(maps:is_key(<<"d">>, ResultDocument)),
    ?assertNot(maps:is_key(<<"a">>, ResultDocument)),
    ?assertNot(maps:is_key(<<"b">>, ResultDocument)),
    '👌'.

updating_existing_document_changes_only_updated_keys(_Config) ->
    DocumentKey = <<"asdf">>,
    OldDocument =
        #{
            '_key' => DocumentKey,
            '🅰️' => 1,
            '🅱️' => 1
        },
    NewDocument =
        #{
            '_key' => DocumentKey,
            '🅱️' => 2,
            c => 2
        },
    {'👌', _} = earangodb:document_create(?CollectionName, OldDocument),
    {'👌', _} = earangodb:document_update(?CollectionName, DocumentKey, NewDocument),
    {'👌', #{
        <<"a">> := 1,
        <<"b">> := 2,
        <<"c">> := 2
    }} =
        earangodb:document_read(?CollectionName, DocumentKey),
    '👌'.

when_deleted_document_cannot_be_read(_Config) ->
    DocumentKey = <<"asdf">>,
    OldDocument =
        #{
            '_key' => DocumentKey,
            '🅰️' => 1,
            '🅱️' => 1
        },
    {'👌', _} = earangodb:document_create(?CollectionName, OldDocument),
    {'👌', _} = earangodb:document_read(?CollectionName, DocumentKey),
    {'👌', _} = earangodb:document_delete(?CollectionName, DocumentKey),
    {'🐛', _} = earangodb:document_read(?CollectionName, DocumentKey).

aql_query_works_for_checking_docs(_Config) ->
    DocumentA =
        #{
            '_key' => <<"doc_a">>,
            '🅱️' => 1,
            c => <<"wrong">>
        },
    DocumentB =
        #{
            '_key' => <<"doc_b">>,
            '🅱️' => 2,
            c => <<"right">>
        },
    DocumentC =
        #{
            '_key' => <<"doc_c">>,
            '🅱️' => 2,
            c => <<"right">>
        },
    {'👌', _} = earangodb:document_create(?CollectionName, DocumentA),
    {'👌', _} = earangodb:document_create(?CollectionName, DocumentB),
    {'👌', _} = earangodb:document_create(?CollectionName, DocumentC),
    {'👌', _} = earangodb:execute_query(<<"FOR d IN MyColecton FILTER d.b == 2 RETURN d.c">>),
    '👌'.
