%% -*- coding: utf-8 -*-
%%
%% Copyright 2016-2017 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%
-module(saci_service).

-include_lib("public_key/include/public_key.hrl").
-include("saci.hrl").

-export([
    get_auth_token/1,
    download_object/5,
    upload_object/5,
    delete_object/3
    ]).

-define(BASE_URL, <<"https://www.googleapis.com/storage/v1/b/">>).
-define(UPLOAD_URL, <<"https://www.googleapis.com/upload/storage/v1/b/">>).
-define(AUTH_URL, "https://www.googleapis.com/oauth2/v4/token").
-define(AUD_URL, <<"https://www.googleapis.com/oauth2/v4/token">>).

-define(GOOGLE_INTERNAL_AUTH_URL, <<"http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token">>).

% read/write is useful for adding and deleting, but that's about it
-define(READ_WRITE_SCOPE, <<"https://www.googleapis.com/auth/devstorage.read_write">>).
% full-control is required for updating/patching existing resources
-define(FULL_CONTROL_SCOPE, <<"https://www.googleapis.com/auth/devstorage.full_control">>).

% Base64 encoding of JSON {"alg":"RS256","typ":"JWT"}
-define(JWT_HEADER, <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9">>).
-define(GRANT_TYPE, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>).

% @doc
%
% Upload an object to the named bucket and return {ok, Object} or {error,
% Reason}, where Object has the updated object properties.
%
-spec upload_object(object(), request_body(), access_token(), list(), integer()) -> {ok, object()} | {error, term()}.
upload_object(Object, RequestBody, Token, Options, Timeout) ->
    BucketName = Object#object.bucket,
    Url = saci_utils:make_url(
        ?UPLOAD_URL, <<BucketName/binary, "/o">>, [{"uploadType", "resumable"}]),
    ReqHeaders = saci_utils:add_auth_header(Token, [
        {<<"Content-Type">>, <<"application/json; charset=UTF-8">>},
        {<<"X-Upload-Content-Type">>, Object#object.contentType},
        {<<"X-Upload-Content-Length">>, Object#object.size}
    ]),
    ReqBody = binary_to_list(jsone:encode({[
        {<<"name">>, Object#object.name},
        % include the md5 so GCP can verify the upload was successful
        {<<"md5Hash">>, Object#object.md5Hash}
    ]})),
    case saci_utils:send_req(post, Url, ReqHeaders, ReqBody) of
        {ok, Status, Headers, _Body} = Response ->
            case Status of
                "200" ->
                    % need to read/skip the body to close the connection
                    UploadUrl = proplists:get_value("Location", Headers),
                    do_upload(UploadUrl, Object, RequestBody, Token, Options, Timeout);
                _ -> Response
            end;
        Error ->
            Error
    end.

% @doc
%
% Perform put request to upload the object to the given upload URL and return {ok, Object} or
% {error, Reason}, where Object has the updated object properties.
%
-spec do_upload(binary(), object(), request_body(), access_token(), list(), integer()) -> {ok, object()} | {error, term()}.
do_upload(Url, Object, RequestBody, Token, Options, Timeout) ->
    ReqHeaders = saci_utils:add_auth_header(Token, [
        {<<"Content-Type">>, Object#object.contentType},
        {<<"Content-Length">>, Object#object.size}
    ]),
    saci_utils:send_req(put, Url, ReqHeaders, RequestBody, Options, Timeout).

% @doc
%
% Retrieve the object and save to the named file.
%
-spec download_object(binary(), binary(), credentials(), list(), integer()) -> ok | {error, term()}.
download_object(BucketName, ObjectName, Token, Options, Timeout) ->
    ON = saci_utils:urlencode(ObjectName),
    UrlPath = <<BucketName/binary, "/o/", ON/binary>>,
    Url = saci_utils:make_url(?BASE_URL, UrlPath, [{"alt", "media"}]),
    ReqHeaders = saci_utils:add_auth_header(Token, []),
    saci_utils:send_req(get, Url, ReqHeaders, Options, Timeout).
    
% @doc
%
% Delete the named object in the named bucket.
%
-spec delete_object(binary(), binary(), credentials()) -> ok | {error, term()}.
delete_object(BucketName, ObjectName, Token) ->
    ON = saci_utils:urlencode(ObjectName),
    Url = <<?BASE_URL/binary, BucketName/binary, "/o/", ON/binary>>,
    ReqHeaders = saci_utils:add_auth_header(Token, []),
    saci_utils:send_req(delete, Url, ReqHeaders).

% @doc
%
% Retrieve a read/write authorization token from the remote service, based
% on the provided credentials, which contains the client email address and
% the PEM encoded private key.
%
-spec get_auth_token(credentials()) -> {ok, access_token()} | {error, term()}.
get_auth_token(use_google_internal_metadata_server) ->
    %% it assumes that workload identity is properly configured
    {ok, _Status, _Headers, _Client} = saci_utils:send_req(get, ?GOOGLE_INTERNAL_AUTH_URL, [{<<"Metadata-Flavor">>, <<"Google">>}]),
    ok; %decode_token_response(Status, Headers, Client);
get_auth_token(Creds) ->
    Now = seconds_since_epoch(),
    % GCP seems to completely ignore the timeout value and always expires
    % in 3600 seconds anyway. Who knows, maybe someday it will work, but
    % you can forget about automated testing of expiration for now.
    Timeout = application:get_env(saci, auth_timeout, 3600),
    ClaimSet = base64:encode(jsone:encode({[
        {<<"iss">>, Creds#credentials.client_email},
        {<<"scope">>, ?FULL_CONTROL_SCOPE},
        {<<"aud">>, ?AUD_URL},
        {<<"exp">>, Now + Timeout},
        {<<"iat">>, Now}
    ]})),
    JwtPrefix = <<?JWT_HEADER/binary, ".", ClaimSet/binary>>,
    PrivateKey = Creds#credentials.private_key,
    Signature = compute_signature(PrivateKey, JwtPrefix),
    Jwt = <<JwtPrefix/binary, ".", Signature/binary>>,
    ReqBody = {form, [{<<"grant_type">>, ?GRANT_TYPE}, {<<"assertion">>, Jwt}]},
    case saci_utils:send_req(post, ?AUTH_URL, [], ReqBody) of    
           {ok, [$2 | _] = _Code, _Headers, Body}    -> decode_token_response(Body);
           {ok, Code, _Headers, _}                   -> {error, {http, Code}};
           {error, Error}                            -> {error, Error};
           _Error                                    -> {error, unknown}
    end.

decode_token_response(Body) ->
    try jsone:decode(Body, [{object_format, proplist}]) of
        Token when is_list(Token) ->
            AccessToken = proplists:get_value(<<"access_token">>, Token),
            TokenType = proplists:get_value(<<"token_type">>, Token),
            {ok, #access_token{access_token=AccessToken, token_type=TokenType}}
    catch
        Error -> Error
    end.

% @doc
%
% Return the seconds since the epoch (1970/1/1 00:00).
%
-spec seconds_since_epoch() -> integer().
seconds_since_epoch() ->
    {Mega, Sec, _Micro} = os:timestamp(),
    Mega * 1000000 + Sec.

% @doc
%
% Compute the SHA256 signature of the given Msg data, using the PEM key
% binary data (which will be decoded and from which the RSA private key
% is extracted). The result is Base64 encoded, appropriate for making
% an authorization request.
%
-spec compute_signature(binary(), binary()) -> binary().
compute_signature(PemKeyBin, Msg) ->
    [PemKeyData] = public_key:pem_decode(PemKeyBin),
    PemKey = public_key:pem_entry_decode(PemKeyData),
    RsaKey = public_key:der_decode('RSAPrivateKey', PemKey#'PrivateKeyInfo'.privateKey),
    base64:encode(public_key:sign(Msg, sha256, RsaKey)).
    % base64:encode(public_key:sign(Msg, sha256, PemKey)).
