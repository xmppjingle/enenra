%% -*- coding: utf-8 -*-
%%
%% Copyright 2016-2017 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%
-module(saci_service).

-export([
    get_auth_token/1,
    download_object/4,
    upload_object/3,
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

-include_lib("public_key/include/public_key.hrl").
-include("saci.hrl").

% @doc
%
% Upload an object to the named bucket and return {ok, Object} or {error,
% Reason}, where Object has the updated object properties.
%
-spec upload_object(object(), request_body(), access_token()) -> {ok, object()} | {error, term()}.
upload_object(Object, RequestBody, Token) ->
    BucketName = Object#object.bucket,
    Url = hackney_url:make_url(
        ?UPLOAD_URL, <<BucketName/binary, "/o">>, [{"uploadType", "resumable"}]),
    ReqHeaders = add_auth_header(Token, [
        {<<"Content-Type">>, <<"application/json; charset=UTF-8">>},
        {<<"X-Upload-Content-Type">>, Object#object.contentType},
        {<<"X-Upload-Content-Length">>, Object#object.size}
    ]),
    ReqBody = binary_to_list(jsone:encode({[
        {<<"name">>, Object#object.name},
        % include the md5 so GCP can verify the upload was successful
        {<<"md5Hash">>, Object#object.md5Hash}
    ]})),
    {ok, Status, Headers, Client} = saci_utils:send_req(post, Url, ReqHeaders, ReqBody),
    case Status of
        200 ->
            % need to read/skip the body to close the connection
            hackney:skip_body(Client),
            UploadUrl = proplists:get_value(<<"Location">>, Headers),
            do_upload(UploadUrl, Object, RequestBody, Token);
        _ -> decode_response(Status, Headers, Client)
    end.

% @doc
%
% Perform put request to upload the object to the given upload URL and return {ok, Object} or
% {error, Reason}, where Object has the updated object properties.
%
-spec do_upload(binary(), object(), request_body(), access_token()) -> {ok, object()} | {error, term()}.
do_upload(Url, Object, RequestBody, Token) ->
    ReqHeaders = add_auth_header(Token, [
        {<<"Content-Type">>, Object#object.contentType},
        {<<"Content-Length">>, Object#object.size}
    ]),
    % Receiving the response after an upload can take a few seconds, so
    % give it a chance to compute the MD5 and such before timing out. Set
    % the timeout rather high as it seems that certain inputs can cause a
    % long delay in response?
    Options = [{recv_timeout, 300000}],
    % Errors during upload are not unusual, so return them gracefully
    % rather than exploding and generating a lengthy crash report.

    %
    % TODO: works on Erlang 19? but not on Erlang 20?
    %
    case saci_utils:send_req(put, Url, ReqHeaders, RequestBody, Options) of
        {ok, Status, Headers, Client} ->
            case decode_response(Status, Headers, Client) of
                {ok, Body} -> {ok, make_object(Body)};
                R0 -> R0
            end;
        R1 -> R1
    end.

% @doc
%
% Retrieve the object and save to the named file.
%
-spec download_object(binary(), binary(), string(), credentials()) -> ok | {error, term()}.
download_object(BucketName, ObjectName, Filename, Token) ->
    ON = hackney_url:urlencode(ObjectName),
    UrlPath = <<BucketName/binary, "/o/", ON/binary>>,
    Url = hackney_url:make_url(?BASE_URL, UrlPath, [{"alt", "media"}]),
    ReqHeaders = add_auth_header(Token, []),
    {ok, Status, Headers, Client} = saci_utils:send_req(get, Url, ReqHeaders),
    case Status of
        200 ->
            {ok, FileHandle} = file:open(Filename, [write]),
            stream_to_file(FileHandle, Client);
        _ -> decode_response(Status, Headers, Client)
    end.

% @doc
%
% Stream the response body of the HTTP request to the opened file. Returns
% ok if successful, or {error, Reason} if not. The file will closed upon
% successful download.
%
-spec stream_to_file(term(), term()) -> ok | {error, term()}.
stream_to_file(FileHandle, Client) ->
    case hackney:stream_body(Client) of
        done -> file:close(FileHandle);
        {ok, Bin} ->
            ok = file:write(FileHandle, Bin),
            stream_to_file(FileHandle, Client);
        R -> R
    end.

% @doc
%
% Delete the named object in the named bucket.
%
-spec delete_object(binary(), binary(), credentials()) -> ok | {error, term()}.
delete_object(BucketName, ObjectName, Token) ->
    ON = hackney_url:urlencode(ObjectName),
    Url = <<?BASE_URL/binary, BucketName/binary, "/o/", ON/binary>>,
    ReqHeaders = add_auth_header(Token, []),
    {ok, Status, Headers, Client} = saci_utils:send_req(delete, Url, ReqHeaders),
    decode_response(Status, Headers, Client).

% @doc
%
% Construct an object record from the given property list.
%
-spec make_object(list()) -> object().
make_object(PropList) ->
    #object{
        id=proplists:get_value(<<"id">>, PropList),
        name=proplists:get_value(<<"name">>, PropList),
        bucket=proplists:get_value(<<"bucket">>, PropList),
        contentType=proplists:get_value(<<"contentType">>, PropList),
        timeCreated=proplists:get_value(<<"timeCreated">>, PropList),
        updated=proplists:get_value(<<"updated">>, PropList),
        storageClass=proplists:get_value(<<"storageClass">>, PropList),
        size=proplists:get_value(<<"size">>, PropList),
        md5Hash=proplists:get_value(<<"md5Hash">>, PropList)
    }.

% @doc
%
% Add the "Authorization" header to those given and return the new list.
%
-spec add_auth_header(access_token(), list()) -> list().
add_auth_header(Token, Headers) ->
    AccessToken = Token#access_token.access_token,
    TokenType = Token#access_token.token_type,
    Authorization = <<TokenType/binary, " ", AccessToken/binary>>,
    Headers ++ [
        {<<"Authorization">>, Authorization}
    ].

% @doc
%
% Based on the response, return {ok, Body}, ok, or {error, Reason}. The
% body is the decoded JSON response from the server. The error
% 'auth_required' indicates a new authorization token must be retrieved. A
% 204 returns 'ok', while a 403 returns {error, forbidden}, 404 returns
% {error, not_found}, 409 returns {error, conflict}.
%
-spec decode_response(integer(), list(), term()) -> {ok, term()} | {error, term()}.
decode_response(400, _Headers, Client) ->
    {ok, Body} = hackney:body(Client),
    {error, Body};
decode_response(401, _Headers, Client) ->
    % need to read/skip the body to close the connection
    hackney:skip_body(Client),
    {error, auth_required};
decode_response(403, _Headers, Client) ->
    % need to read/skip the body to close the connection
    hackney:skip_body(Client),
    {error, forbidden};
decode_response(404, _Headers, Client) ->
    % need to read/skip the body to close the connection
    hackney:skip_body(Client),
    {error, not_found};
decode_response(409, _Headers, Client) ->
    % need to read/skip the body to close the connection
    hackney:skip_body(Client),
    {error, conflict};
decode_response(Ok, _Headers, Client) when Ok == 200; Ok == 201 ->
    {ok, Body} = hackney:body(Client),
    try jsone:decode(Body, [{object_format, proplist}]) of
        Results -> {ok, Results}
    catch
        Error -> Error
    end;
decode_response(204, _Headers, Client) ->
    % need to read/skip the body to close the connection
    hackney:skip_body(Client),
    ok;
decode_response(_Status, _Headers, Client) ->
    {ok, Body} = hackney:body(Client),
    try jsone:decode(Body, [{object_format, proplist}]) of
        Results -> {ok, Results}
    catch
        Error -> Error
    end.

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