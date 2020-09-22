-module(saci_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("../include/saci.hrl").

setup_test() ->
    [application:ensure_all_started(A) || A <- [ibrowse, ssl] ].

gcp_auth_upload_test() ->
	?debugFmt("Home Path: ~p ~n", [  string:trim(os:cmd("cd ~;pwd")) ]),
	File = "./test/saci_test_data/Saci_Perere_por_Marconi.jpg",
	Name = <<"Saci_Perere_por_Marconi.jpg">>,
	Bucket = <<"test-bucket">>,
	ConfigFile = string:trim(os:cmd("cd ~;pwd")) ++ "/gcp.json",
	{ok, Creds} = saci_utils:load_credentials(ConfigFile),
	{ok, Token} = saci_service:get_auth_token(Creds),
	?debugFmt("Fetched oauth2 Token ~p.~n", [Token]),

	{ok, Md5} = saci_utils:compute_md5(File),
	Size = filelib:file_size(File),
	
	saci_service:delete_object(Bucket, Name, Token),

	{ok, "200", _, _} = saci_service:upload_object(#object{ name = Name, bucket = Bucket, contentType = <<"image/jpeg">>, md5Hash = Md5, size = Size}, {file, filename:absname(File)}, Token),
	
	?debugFmt("Upload of ~p Completed.~n", [File]),
	
	{ok, "204", _, _} = saci_service:delete_object(Bucket, Name, Token),
	
	?debugFmt("Deletion of ~p/~p Completed.~n", [Bucket, Name]).
