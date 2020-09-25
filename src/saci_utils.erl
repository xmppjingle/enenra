%% -*- erlang -*-
%%%
%%% This file is part of hackney_lib released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2012-2015 Benoît Chesneau <benoitc@e-engura.org>
%%% Copyright (c) 2011, Magnus Klaar <magnus.klaar@gmail.com>
%%%

%% @doc module to manage URLs.

-module(saci_utils).

-include_lib("public_key/include/public_key.hrl").
-include("saci.hrl").

-export([
  load_credentials/1,
  parse_credentials/1
  ]).

-export([
	send_req/5,
	send_req/4,
  send_req/3
  ]).

-export([
	urlencode/1,
  make_url/3,
  compute_md5/1,
  mime_type/1
	]).

% @doc
%
% Load the credentials for the given file, which is assumed to be a JSON
% file containing the client email address, project identifier, private key
% in PEM format, as well as other properties.
%
-spec load_credentials(string()) -> {ok, credentials()}.
load_credentials(Filepath) ->
    {ok, JsonBin} = file:read_file(Filepath),
    parse_credentials(JsonBin).

% @doc
%
% Parse the credentials for the given binary Content, which is assumed to be a JSON
% containing the client email address, project identifier, private key
% in PEM format, as well as other properties.
%
-spec parse_credentials(binary()) -> {ok, credentials()}.
parse_credentials(JsonBin) when is_binary(JsonBin) ->
    Creds = jsone:decode(JsonBin, [{object_format, proplist}]),
    {ok, #credentials{
        type=proplists:get_value(<<"type">>, Creds),
        project_id=proplists:get_value(<<"project_id">>, Creds),
        private_key_id=proplists:get_value(<<"private_key_id">>, Creds),
        private_key=proplists:get_value(<<"private_key">>, Creds),
        client_email=proplists:get_value(<<"client_email">>, Creds),
        client_id=proplists:get_value(<<"client_id">>, Creds)
    }}.

%% @doc URL encode a string binary.
% -spec urlencode(binary() | string()) -> binary().
urlencode(Bin) ->
  urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.
% -spec urlencode(binary() | string(), [qs_opt()]) -> binary().
urlencode(Bin, Opts) ->
  Plus = not proplists:get_value(noplus, Opts, false),
  Upper = proplists:get_value(upper, Opts, false),
  urlencode(to_binary(Bin), <<>>, Plus, Upper).

% -spec urlencode(binary(), binary(), boolean(), boolean()) -> binary().
urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
  if	C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
    C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
    C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
    C =:= $.; C =:= $-; C =:= $~; C =:= $_; C =:= $*; C =:= $@ ->
      urlencode(Rest, <<Acc/binary, C>>, P, U);
    C =:= $(; C =:= $); C =:= $!; C =:= $$ ->
      urlencode(Rest, <<Acc/binary, C>>, P, U);
    C =:= $ , Plus ->
      urlencode(Rest, <<Acc/binary, $+>>, P, U);
    true ->
      H = C band 16#F0 bsr 4, L = C band 16#0F,
      H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
      L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
      urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
  end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
  Acc.

-spec tohexu(byte()) -> byte().
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 16 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 16 -> $a + C - 10.

to_binary(V) when is_list(V) ->
  list_to_binary(V);
to_binary(V) when is_atom(V) ->
  atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
  list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
  V.

join([], _Separator) ->
  <<>>;
join([S], _separator) ->
  S;
join(L, Separator) ->
  iolist_to_binary(join(lists:reverse(L), Separator, [])).

join([], _Separator, Acc) ->
  Acc;
join([S | Rest], Separator, []) ->
  join(Rest, Separator, [S]);
join([S | Rest], Separator, Acc) ->
  join(Rest, Separator, [S, Separator | Acc]).

%% @doc Encode query properties to binary.
% -spec qs(qs_vals()) -> binary().
qs(KVs) ->
  qs(KVs, []).

%% @doc Encode query properties to binary.
%% Opts are passed to {@link urlencode/2.}
% -spec qs(qs_vals(), [qs_opt()]) -> binary().
qs(KVs, Opts) ->
  qs(KVs, Opts, []).

qs([], _Opts, Acc) ->
  join(lists:reverse(Acc), <<"&">>);
qs([{K, V}|R], Opts, Acc) ->
  K1 = urlencode(K, Opts),
  V1 = urlencode(V, Opts),
  Line = << K1/binary, "=", V1/binary >>,
  qs(R, Opts, [Line | Acc]).

%% @doc encode a list of properties in a form.
encode_form(KVs) ->
  Lines = qs(KVs),
  CTypeHeaders = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
  {erlang:byte_size(Lines), CTypeHeaders, Lines}.

%% @doc Construct an URL from a base URL, a path and a list of
%% properties to give to the URL.
make_url(Url, Path, Query) when is_list(Query) ->
  %% a list of properties has been passed
  make_url(Url, Path, qs(Query));
make_url(Url, Path, Query) when is_binary(Path) ->
  make_url(Url, [Path], Query);
make_url(Url, PathParts, Query) when is_binary(Query) ->
  %% create path
  PathParts1 = [fix_path(P) || P <- PathParts, P /= "", P /= "/" orelse P /= <<"/">>],
  Path = join([<<>> | PathParts1], <<"/">>),

  %% initialise the query
  Query1 = case Query of
             <<>> -> <<>>;
             _ -> << "?", Query/binary >>
           end,

  %% make the final uri
  iolist_to_binary([fix_path(Url), Path, Query1]).

fix_path(Path) when is_list(Path) ->
  fix_path(list_to_binary(Path));
fix_path(<<>>) ->
  <<>>;
fix_path(<<"/", Path/binary>>) ->
  fix_path(Path);
fix_path(Path) ->
  case binary:part(Path, {size(Path), -1}) of
    <<"/">> -> binary:part(Path, {0, size(Path) - 1});
    _ -> Path
  end.

% HTTP Client Adapters

send_req(Method, URL, Headers) ->
  send_req(Method, URL, Headers, <<>>, []).

send_req(Method, URL, Headers, ReqBody) ->
	send_req(Method, URL, Headers, ReqBody, []).

send_req(put, URL, Headers, {file, Path}, Options) ->
  {ok, Bin} = file:read_file(Path),
  send_req(put, URL, Headers, Bin, Options ++ [{response_format, binary}]);
send_req(post, URL, Headers, {form, PropListParams}, Options) ->
	{_Size, CHeaders, ReqBody} = encode_form(PropListParams),
	send_req(post, URL, CHeaders ++ Headers, ReqBody, Options ++ [{response_format, binary}]);
send_req(Method, URL, Headers, ReqBody, Options) ->
	ibrowse:send_req(to_str(URL), Headers, Method, ReqBody, Options).
	% {ok, Status, Headers, Client} = ibrowse:send_req(?AUTH_URL, [], post, ReqBody, []),

to_str(Num) when is_integer(Num) -> integer_to_list(Num);
to_str(Str) when is_list(Str) -> Str;
to_str(Atom)  when is_atom(Atom) -> atom_to_list(Atom);
to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin).

% @doc
%
% Compute the MD5 checksum for the named file, returning the Base64 encoded
% result. This value can be given in the upload request and Google Cloud
% Storage will verify the upload was successful by comparing the checksum
% with its own computation.
%
-spec compute_md5(Filename) -> {ok, Digest} | {error, Reason} when
    Filename :: string(),
    Digest :: string(),
    Reason :: term().
compute_md5(Filename) ->
    {ok, Filehandle} = file:open(Filename, [read, binary, read_ahead]),
    Context = erlang:md5_init(),
    case compute_md5(Filehandle, Context) of
        {ok, Digest} -> {ok, base64:encode(Digest)};
        R -> R
    end.

% @doc
%
% Helper function that recursively computes the MD5 of the opened file in
% 64KB chunks. The file will be closed upon successful completion.
%
compute_md5(Filehandle, Context) ->
    case file:read(Filehandle, 65536) of
        {ok, Data} ->
            NewContext = erlang:md5_update(Context, Data),
            compute_md5(Filehandle, NewContext);
        eof ->
            case file:close(Filehandle) of
                ok -> {ok, erlang:md5_final(Context)};
                RR -> RR
            end;
        R -> R
    end.

mime_type(FileName) ->
    "." ++ Extension = filename:extension(FileName),
    MimeTypes = mime_types(),
    proplists:get_value(Extension, MimeTypes, undefined);
mime_type(_) ->
    undefined.