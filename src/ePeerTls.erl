-module(ePeerTls).

-export([start/4]).

-include_lib("public_key/include/public_key.hrl").

start(Module, Peer, Host, Port) ->
    case read_hosts() of
        {ok, Hosts} ->
            Pin = maps:get({Host, Port}, Hosts, undefined),
            connect(Module, Peer, Host, Port, Pin);
        {error, Reason} ->
            {error, {known_hosts, Reason}}
    end.

connect(Module, Peer, Host, Port, undefined) ->
    Options = ca_options(),
    case probe(Host, Port, Options) of
        {ok, _Cert} -> ePort:start(Module, Host, Port, Options);
        {error, _Reason} -> trust_new(Module, Peer, Host, Port)
    end;
connect(Module, Peer, Host, Port, Pin) ->
    Options = pin_options(Pin),
    case probe(Host, Port, Options) of
        {ok, _Cert} -> ePort:start(Module, Host, Port, Options);
        {error, Reason} ->
            warn_changed(Peer, Host, Port, Pin),
            {error, Reason}
    end.

trust_new(Module, Peer, Host, Port) ->
    case probe(Host, Port, [{verify, verify_none}]) of
        {ok, Cert} ->
            Pin     = crypto:hash(sha256, Cert),
            Options = pin_options(Pin),
            approve_new(Module, Peer, Host, Port, Pin, Options);
        Error -> Error
    end.

approve_new(Module, Peer, Host, Port, Pin, Options) ->
    case probe(Host, Port, Options) of
        {ok, _Cert} ->
            remember_and_connect(Module, Peer, Host, Port, Pin, Options);
        Error -> Error
    end.

remember_and_connect(Module, Peer, Host, Port, Pin, Options) ->
    case confirm(Peer, Host, Port, undefined, Pin) of
        true ->
            case remember(Host, Port, Pin) of
                ok -> ePort:start(Module, Host, Port, Options);
                Error -> Error
            end;
        false -> {error, untrusted_certificate}
    end.

warn_changed(Peer, Host, Port, OldPin) ->
    case probe(Host, Port, [{verify, verify_none}]) of
        {ok, Cert} ->
            Pin = crypto:hash(sha256, Cert),
            case Pin =:= OldPin of
                true -> ok;
                false -> confirm(Peer, Host, Port, OldPin, Pin)
            end;
        _ -> ok
    end.

probe(Host, Port, Options) ->
    case ssl:connect(Host, Port, Options, 10000) of
        {ok, Socket} ->
            Result = ssl:peercert(Socket),
            ssl:close(Socket),
            Result;
        Error -> Error
    end.

ca_options() ->
    [{verify, verify_peer},
     {cacerts, public_key:cacerts_get()},
     {customize_hostname_check,
      [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}].

pin_options(Pin) ->
    [{verify_fun, {fun verify_pin/3, Pin}},
     {reuse_sessions, false}, {session_tickets, disabled} | ca_options()].

verify_pin(Cert, {bad_cert, unknown_ca}, Pin) ->
    check_pin(Cert, Pin);
verify_pin(Cert, {bad_cert, selfsigned_peer}, Pin) ->
    check_pin(Cert, Pin);
verify_pin(Cert, {bad_cert, hostname_check_failed}, Pin) ->
    check_pin(Cert, Pin);
verify_pin(_Cert, {bad_cert, Reason}, _Pin) ->
    {fail, Reason};
verify_pin(_Cert, {extension, _Extension}, Pin) ->
    {unknown, Pin};
verify_pin(_Cert, valid, Pin) ->
    {valid, Pin};
verify_pin(Cert, valid_peer, Pin) ->
    check_pin(Cert, Pin).

check_pin(Cert, Pin) when not is_binary(Cert) ->
    check_pin(public_key:pkix_encode('OTPCertificate', Cert, otp), Pin);
check_pin(Cert, Pin) ->
    case crypto:hash(sha256, Cert) =:= Pin of
        true -> check_validity(Cert, Pin);
        false -> {fail, certificate_changed}
    end.

check_validity(Cert, Pin) ->
    #'OTPCertificate'{tbsCertificate = Tbs} =
        public_key:pkix_decode_cert(Cert, otp),
    #'Validity'{notBefore = From, notAfter = To} =
        Tbs#'OTPTBSCertificate'.validity,
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case Now >= certificate_time(From) andalso Now =< certificate_time(To) of
        true -> {valid, Pin};
        false -> {fail, cert_expired}
    end.

certificate_time({utcTime, [Year1, Year2 | Rest]}) ->
    Year   = list_to_integer([Year1, Year2]),
    Prefix = case Year >= 50 of true -> "19"; false -> "20" end,
    certificate_time({generalTime, Prefix ++ [Year1, Year2 | Rest]});
certificate_time({generalTime, Value}) ->
    <<Year:4/binary, Month:2/binary, Day:2/binary, Hour:2/binary,
      Minute:2/binary, Second:2/binary, "Z">> = list_to_binary(Value),
    calendar:datetime_to_gregorian_seconds(
      {{binary_to_integer(Year), binary_to_integer(Month),
        binary_to_integer(Day)},
       {binary_to_integer(Hour), binary_to_integer(Minute),
        binary_to_integer(Second)}}).

confirm(Peer, Host, Port, OldPin, Pin) ->
    Request = {trustPeerCertificate, Peer, Host, Port,
               fingerprint(OldPin), fingerprint(Pin)},
    try wx_object:call(eTodo, Request, infinity) of
        true -> true;
        _ -> false
    catch
        exit:_ ->
            logger:warning("Peer certificate needs approval: ~s:~p SHA256 ~s",
                           [Host, Port, fingerprint(Pin)]),
            false
    end.

fingerprint(undefined) -> undefined;
fingerprint(Pin) ->
    string:join([lists:flatten(io_lib:format("~2.16.0B", [Byte]))
                 || <<Byte>> <= Pin], ":").

hosts_file() ->
    Dir = application:get_env(mnesia, dir,
                              filename:basedir(user_data, "eTodo")),
    filename:join(Dir, "peer_known_hosts").

read_hosts() ->
    case file:consult(hosts_file()) of
        {ok, [Hosts]} when is_map(Hosts) -> {ok, Hosts};
        {ok, _} -> {error, invalid_known_hosts};
        {error, enoent} -> {ok, #{}};
        {error, Reason} -> {error, Reason}
    end.

remember(Host, Port, Pin) ->
    global:trans({{?MODULE, known_hosts}, self()},
                 fun() -> save_host(Host, Port, Pin) end).

save_host(Host, Port, Pin) ->
    case read_hosts() of
        {ok, Hosts} ->
            case maps:get({Host, Port}, Hosts, Pin) of
                Pin -> write_hosts(Hosts#{{Host, Port} => Pin});
                _ -> {error, certificate_changed}
            end;
        Error -> Error
    end.

write_hosts(Hosts) ->
    File = hosts_file(),
    Tmp  = File ++ ".tmp",
    Data = io_lib:format("~p.~n", [Hosts]),
    Result = filelib:ensure_dir(File),
    write_hosts(Result, File, Tmp, Data).

write_hosts(ok, File, Tmp, Data) ->
    case file:write_file(Tmp, Data, [exclusive]) of
        ok ->
            Result = case file:change_mode(Tmp, 8#600) of
                         ok -> file:rename(Tmp, File);
                         Error -> Error
                     end,
            file:delete(Tmp),
            Result;
        Error -> Error
    end;
write_hosts(Error, _File, _Tmp, _Data) -> Error.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pin_verification_test() ->
    {Cert, _Options} = test_certificate(),
    Pin = crypto:hash(sha256, Cert),
    ?assertEqual({valid, Pin}, verify_pin(Cert, valid_peer, Pin)),
    Decoded = public_key:pkix_decode_cert(Cert, otp),
    ?assertEqual({valid, Pin}, verify_pin(Decoded, valid_peer, Pin)),
    ?assertEqual({valid, Pin},
                 verify_pin(Cert, {bad_cert, selfsigned_peer}, Pin)),
    ?assertEqual({fail, certificate_changed},
                 verify_pin(Cert, valid_peer, <<0:256>>)),
    ?assertEqual({fail, cert_expired},
                 verify_pin(Cert, {bad_cert, cert_expired}, Pin)).

expired_pin_test() ->
    {Cert, _Options} = test_certificate(),
    Decoded = public_key:pkix_decode_cert(Cert, otp),
    Tbs     = Decoded#'OTPCertificate'.tbsCertificate,
    Validity = #'Validity'{notBefore = {utcTime, "200101000000Z"},
                           notAfter = {utcTime, "210101000000Z"}},
    Expired = public_key:pkix_encode(
                'OTPCertificate',
                Decoded#'OTPCertificate'{tbsCertificate =
                    Tbs#'OTPTBSCertificate'{validity = Validity}}, otp),
    Pin = crypto:hash(sha256, Expired),
    ?assertEqual({fail, cert_expired},
                 verify_pin(Expired, {bad_cert, unknown_ca}, Pin)).

self_signed_tls_test_() ->
    [{timeout, 30, fun() -> self_signed_tls('tlsv1.3') end},
     {timeout, 30, fun() -> self_signed_tls('tlsv1.2') end}].

self_signed_tls(Version) ->
    {ok, _} = application:ensure_all_started(ssl),
    {Cert, Options} = test_certificate(),
    {ok, Listen} = ssl:listen(0, [{active, false},
                                 {versions, [Version]} | Options]),
    {ok, {_Address, Port}} = ssl:sockname(Listen),
    Server = spawn(fun() -> test_accept(Listen) end),
    Pin = crypto:hash(sha256, Cert),
    try
        ?assertMatch({error, _}, probe("localhost", Port, ca_options())),
        ?assertEqual({ok, Cert}, probe("localhost", Port, pin_options(Pin))),
        ?assertMatch({error, _},
                     probe("localhost", Port, pin_options(<<0:256>>))),
        ?assertEqual({ok, Cert}, probe("localhost", Port, pin_options(Pin)))
    after
        ssl:close(Listen),
        exit(Server, kill)
    end.

known_hosts_test() ->
    Dir = filename:join(
            [os:getenv("TMPDIR", "/tmp"),
             "etodo-peer-tls-" ++
                 integer_to_list(erlang:unique_integer([positive]))]),
    Previous = application:get_env(mnesia, dir),
    application:set_env(mnesia, dir, Dir),
    Pin = crypto:hash(sha256, <<"test certificate">>),
    try
        ?assertEqual({ok, #{}}, read_hosts()),
        ?assertEqual(ok, remember("localhost", 19000, Pin)),
        ?assertEqual({ok, #{{"localhost", 19000} => Pin}}, read_hosts()),
        ?assertEqual({error, certificate_changed},
                     remember("localhost", 19000, <<0:256>>)),
        ?assertEqual(ok, remember("other-host", 19000, Pin)),
        {ok, Hosts} = read_hosts(),
        ?assertEqual(2, map_size(Hosts)),
        ?assertEqual(ok, file:write_file(hosts_file(), <<"not a term">>)),
        ?assertMatch({error, _}, read_hosts())
    after
        file:delete(hosts_file()),
        file:del_dir(Dir),
        case Previous of
            {ok, Value} -> application:set_env(mnesia, dir, Value);
            undefined -> application:unset_env(mnesia, dir)
        end
    end.

test_certificate() ->
        Crypto = [{key, {namedCurve, secp256r1}}, {digest, sha256}],
    #{server_config := Options} =
        public_key:pkix_test_data(
                    #{server_chain => #{root => Crypto, peer => Crypto},
            client_chain => #{root => [], peer => []}}),
    {proplists:get_value(cert, Options),
     proplists:delete(cacerts, Options)}.

test_accept(Listen) ->
    case ssl:transport_accept(Listen, 10000) of
        {ok, Socket} ->
            case ssl:handshake(Socket, 10000) of
                {ok, Connected} ->
                    ssl:recv(Connected, 0, 1000),
                    ssl:close(Connected);
                _ -> ok
            end,
            test_accept(Listen);
        _ -> ok
    end.
-endif.