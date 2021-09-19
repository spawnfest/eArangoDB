-module(earangodb_documents).

-export([
    create/2,
    read/2,
    update/3,
    replace/3,
    delete/2
]).

-spec create(CollectionName :: binary(), Document :: map()) -> {ok, ResponseBody :: map()} | {error, Reason :: term()}.
create(CollectionName, Document = #{}) ->
    Url = earangodb_http_client:build_url("/_api/document/" ++ earangodb_http_client:maybe_binary_to_lst(CollectionName)),
    Headers = earangodb_http_client:build_headers(),
    Body = jiffy:encode(Document),
    Response = earangodb_http_client:send_request_and_process_response(post, Url, Headers, Body),
    earangodb_http_client:unwrap_response(Response).

-spec read(CollectionName :: binary(), DocumentKey :: binary()) -> {ok, Document :: map()} | {error, Reason :: term()}.
read(CollectionName, DocumentKey) ->
    Url = earangodb_http_client:build_url("/_api/document/" ++ earangodb_http_client:maybe_binary_to_lst(CollectionName) ++ "/" ++ earangodb_http_client:maybe_binary_to_lst(DocumentKey)),
    Headers = earangodb_http_client:build_headers(),
    Response = earangodb_http_client:send_request_and_process_response(get, Url, Headers, <<"">>),
    earangodb_http_client:unwrap_response(Response).

-spec update(CollectionName :: binary(), DocumentKey :: binary(), Document :: map()) -> {ok, Response :: map()} | {error, Reason :: term()}.
update(CollectionName, DocumentKey, Document) ->
    update_or_replace_or_delete(CollectionName, DocumentKey, Document, patch).
    

-spec replace(CollectionName :: binary(), DocumentKey :: binary(), Document :: map()) -> {ok, Response :: map()} | {error, Reason :: term()}.
replace(CollectionName, DocumentKey, Document) ->
    update_or_replace_or_delete(CollectionName, DocumentKey, Document, put).

-spec delete(CollectionName :: binary(), DocumentKey :: binary()) -> {ok, Response :: map()} | {error, Reason :: term()}.
delete(CollectionName, DocumentKey) ->
    update_or_replace_or_delete(CollectionName, DocumentKey, #{}, delete).

update_or_replace_or_delete(CollectionName, DocumentKey, Document, Method) ->
    Url = earangodb_http_client:build_url("/_api/document/" ++ earangodb_http_client:maybe_binary_to_lst(CollectionName) ++ "/" ++ earangodb_http_client:maybe_binary_to_lst(DocumentKey)),
    Headers = earangodb_http_client:build_headers(),
    Body = jiffy:encode(Document),
    Response = earangodb_http_client:send_request_and_process_response(Method, Url, Headers, Body),
    earangodb_http_client:unwrap_response(Response).

