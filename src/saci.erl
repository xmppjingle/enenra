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
    upload_file/3, 
    download_object/4, 
    delete_object/3
    ]).

% @doc
%
% Upload the file identified by Filename, with the properties given by
% Object, to the bucket named in the Object#bucket field. The returned
% Object value will have the updated properties.
%
-spec upload_file(Filename, Object, Credentials) -> {ok, Object} | {error, Reason} when
    Filename :: string(),
    Object :: object(),
    Credentials :: credentials(),
    Reason :: term().
upload_file(Filename, Object, Credentials) ->
    RequestBody = {file, Filename},
    saci_service:upload_object(Object, RequestBody, Credentials).

% @doc
%
% Retrieve the object named ObjectName in the bucket named BucketName,
% storing the result in the file named Filename. Returns 'ok' on success,
% or {error, Reason} if error.
%
-spec download_object(BucketName, ObjectName, Filename, Credentials) -> ok | {error, Reason} when
    Filename :: string(),
    BucketName :: binary(),
    ObjectName :: binary(),
    Credentials :: credentials(),
    Reason :: term().
download_object(BucketName, ObjectName, Filename, Credentials) ->
    saci_service:download_object(BucketName, ObjectName, Filename, Credentials).

% @doc
%
% Delete the object named ObjectName in the bucket named BucketName,
% returning 'ok' if successful, or {error, Reason} if an error occurred.
%
-spec delete_object(BucketName, ObjectName, Credentials) -> ok | {error, Reason} when
    BucketName :: binary(),
    ObjectName :: binary(),
    Credentials :: credentials(),
    Reason :: term().
delete_object(BucketName, ObjectName, Credentials) ->
    saci_service:delete_object(BucketName, ObjectName, Credentials).

