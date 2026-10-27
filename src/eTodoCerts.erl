%%%-------------------------------------------------------------------
%%% @doc
%%% Creates a self-signed TLS certificate and private key when none
%%% exists, so a fresh install can start without extra setup.
%%% Existing files are never touched.
%%% @end
%%%-------------------------------------------------------------------
-module(eTodoCerts).

-export([ensure/1]).

-define(VALID_YEARS, 10).

%%--------------------------------------------------------------------
%% @doc
%% Makes sure server_cert.pem and server_key.pem exist in Dir.
%% Nothing is created if either file is already there.
%%
%% @spec ensure(Dir) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
ensure(Dir) ->
    CertFile = filename:join(Dir, "server_cert.pem"),
    KeyFile  = filename:join(Dir, "server_key.pem"),
    global:trans({?MODULE, Dir},
                 fun() ->
                         case filelib:is_file(CertFile)
                             orelse filelib:is_file(KeyFile) of
                             true  -> ok;
                             false -> generate(CertFile, KeyFile)
                         end
                 end).

generate(CertFile, KeyFile) ->
    try
        {{Y, M, _}, _} = calendar:universal_time(),
        Valid = {{Y - 1, M, 1}, {Y + ?VALID_YEARS, M, 1}},
        Key   = {namedCurve, secp256r1},
        Opts  = [{key, Key}, {digest, sha256}, {validity, Valid}],
        #{server_config := Conf} =
            public_key:pkix_test_data(
              #{server_chain => #{root => Opts, peer => Opts},
                client_chain => #{root => [], peer => []}}),
        Cert   = proplists:get_value(cert, Conf),
        {KeyType, KeyDer} = proplists:get_value(key, Conf),
        [CA | _] = proplists:get_value(cacerts, Conf),
        CertPem = public_key:pem_encode(
                    [{'Certificate', Cert, not_encrypted},
                     {'Certificate', CA, not_encrypted}]),
        KeyPem  = public_key:pem_encode([{KeyType, KeyDer, not_encrypted}]),
        ok = write_atomic(KeyFile, KeyPem, 8#600),
        ok = write_atomic(CertFile, CertPem, 8#644),
        logger:notice("Created self-signed TLS certificate ~s", [CertFile]),
        ok
    catch
        Class:Reason ->
            {error, {certificate_generation_failed, Class, Reason}}
    end.

write_atomic(File, Data, Mode) ->
    Tmp = File ++ ".tmp",
    ok = file:write_file(Tmp, Data),
    ok = file:change_mode(Tmp, Mode),
    file:rename(Tmp, File).
