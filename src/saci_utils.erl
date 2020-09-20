-module(saci_utils).

-export([
	send_req/5,
	send_req/4,
	urlencode/1
	]).

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

send_req(Method, URL, Headers, ReqBody) ->
	send_req(Method, URL, Headers, ReqBody, []).

send_req(post, URL, Headers, {form, PropListParams}, Options) ->
	{_Size, CHeaders, ReqBody} = encode_form(PropListParams),
	ibrowse:send_req(URL, CHeaders ++ Headers, post, ReqBody, Options ++ [{response_format, binary}]);
send_req(Method, URL, Headers, ReqBody, Options) ->
	ibrowse:send_req(URL, Headers, Method, ReqBody, Options).
	% {ok, Status, Headers, Client} = ibrowse:send_req(?AUTH_URL, [], post, ReqBody, []),