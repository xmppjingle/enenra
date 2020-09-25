%% -*- coding: utf-8 -*-
%%
%% Copyright 2016-2018 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%

%
% @author Nathan Fiedler <nathanfiedler@fastmail.fm>
% @copyright 2016-2018 Nathan Fiedler
% @version 0.3.0
% @doc A library for interfacing with Google Cloud Storage.
%
% `saci' is an Erlang/OTP library for creating buckets, uploading objects,
% and otherwise managing said resources on Google Cloud Storage. This is
% done via the HTTP/JSON API described in the Google Cloud Storage
% documentation ([https://cloud.google.com/storage/docs/json_api/]).
%
% Regarding the authorization scope used for interacting with Google Cloud
% Storage, the `full-control' scope is used, as that allows modifying
% existing resources. The `read-write' scope only allows creating and
% deleting, but not patching.
%
-module(saci).

-include("saci.hrl").

-export([
    upload_file/6,
    download_file/6,
    delete_object/3
    ]).

-export([start/0, start_link/0]).

start() ->
    application:ensure_all_started(mimetypes),
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    application:ensure_all_started(mimetypes),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% @doc
%
% Upload the file identified by Filename, with the properties given by
% Object, to the bucket named in the Object#bucket field. The returned
% Object value will have the updated properties.
%
-spec upload_file(Filename, BucketName, ObjectName, Token, Options, Timeout) -> {ok, Object} | {error, Reason} when
    Filename :: string(),
    BucketName :: binary(),
    ObjectName :: binary(),
    Token :: access_token(),
    Options :: list(),
    Timeout :: integer(),
    Object :: object(),
    Reason :: term().
upload_file(Filename, BucketName, ObjectName, Token, Options, Timeout) ->
    RequestBody = {file, Filename},
    {ok, Md5} = saci_utils:compute_md5(Filename),
    Size = filelib:file_size(Filename),
    [ContentType] = mimetypes:filename(Filename),
    Object = #object{ name = ObjectName, bucket = BucketName, contentType = ContentType, md5Hash = Md5, size = Size},
    saci_service:upload_object(Object, RequestBody, Token, Options, Timeout).

% @doc
%
% Retrieve the object named ObjectName in the bucket named BucketName,
% storing the result in the file named Filename. Returns 'ok' on success,
% or {error, Reason} if error.
%
-spec download_file(BucketName, ObjectName, Filename, Token, Options, Timeout) -> ok | {error, Reason} when
    Filename :: string(),
    BucketName :: binary(),
    ObjectName :: binary(),
    Token :: access_token(),
    Options :: list(),
    Timeout :: integer(),
    Reason :: term().
download_file(BucketName, ObjectName, Filename, Token, Options, Timeout) ->
    saci_service:download_object(BucketName, ObjectName, Filename, Token, [{save_response_to_file, Filename}] ++ Options, Timeout).

% @doc
%
% Delete the object named ObjectName in the bucket named BucketName,
% returning 'ok' if successful, or {error, Reason} if an error occurred.
%
-spec delete_object(BucketName, ObjectName, Token) -> ok | {error, Reason} when
    BucketName :: binary(),
    ObjectName :: binary(),
    Token :: access_token(),
    Reason :: term().
delete_object(BucketName, ObjectName, Token) ->
    saci_service:delete_object(BucketName, ObjectName, Token).

