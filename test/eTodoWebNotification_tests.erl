-module(eTodoWebNotification_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

notification_destination_test_() ->
    {timeout, 30, fun notification_destination/0}.

notification_destination() ->
    {Server, Auth, Database, Dir, Port, Old} = start_fixture(),
    try
        lists:foreach(fun(Kind) -> check_destination(Port, Kind) end,
                      ["chat", "reminders"]),
        ?assertEqual("/", login_location(Port, [])),
        BadCookie = "etodo_messages_" ++ integer_to_list(Port) ++
            "=https://other.example/",
        ?assertEqual("/", login_location(Port, [{"cookie", BadCookie}])),
        {200, RelayHeaders, _} = web_get(
                                   Port, "/messages?kind=reminders",
                                   [{"host", "relay.example:9005"}]),
        RelayCookie = cookie_pair("etodo_messages_9005", RelayHeaders),
        ?assertEqual(
           "/messages?kind=reminders",
           login_location(Port, [{"host", "relay.example:9005"},
                                 {"cookie", RelayCookie}])),
        ?assertEqual(
           "/", login_location(Port, [{"host", "relay.example:9003"},
                                      {"cookie", RelayCookie}])),
        check_google_destination(Port)
    after
        Ref = monitor(process, Server),
        unlink(Server),
        exit(Server, shutdown),
        receive {'DOWN', Ref, process, Server, shutdown} -> ok end,
        gen_server:stop(Auth),
        DbRef = monitor(process, Database),
        Database ! stop,
        receive {'DOWN', DbRef, process, Database, normal} -> ok end,
        inets:stop(httpc, etodo_notification_test),
        inets:stop(httpc, eTodoOidcDirect),
        ok = file:del_dir_r(Dir),
        [case Value of
             {ok, Previous} -> application:set_env(eTodo, Key, Previous);
             undefined -> application:unset_env(eTodo, Key)
         end || {Key, Value} <- Old]
    end.

check_destination(Port, Kind) ->
    Path = "/messages?kind=" ++ Kind,
    {200, LoginHeaders, _} = web_get(Port, Path, []),
    ReturnName = "etodo_messages_" ++ integer_to_list(Port),
    ReturnCookie = cookie_pair(ReturnName, LoginHeaders),
    {401, _, ErrorPage} = web_post(
                           Port, "/login",
                           "username=test-user&password=incorrect",
                           [{"cookie", ReturnCookie}]),
    ?assertNotEqual(nomatch,
                    binary:match(ErrorPage, <<"Invalid username or password">>)),
    {303, Headers, _} = web_post(
                         Port, "/login", credentials(),
                         [{"cookie", ReturnCookie}]),
    ?assertEqual(Path, proplists:get_value("location", Headers)),
    Cleared = cookie_header(ReturnName, Headers),
    ?assertNotEqual(nomatch, string:find(Cleared, "Max-Age=0")),
    SessionName = "etodo_session_" ++ integer_to_list(Port),
    Session = cookie_pair(SessionName, Headers),
    {200, _, Page} = web_get(Port, Path, [{"cookie", Session}]),
    ?assertNotEqual(
       nomatch,
       binary:match(Page, list_to_binary("data-selected=\"" ++ Kind ++ "\""))).

start_fixture() ->
    ok = code:add_paths(filelib:wildcard("web/backend/build/dev/erlang/*/ebin")),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(etodo_web),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = inets:start(httpc, [{profile, etodo_notification_test}]),
    Settings = [{user, "test-user"},
                {mistGoogleClientId, "test-client"},
                {mistGoogleClientSecret, "test-secret"},
                {mistGoogleRedirectUri, "https://example.test/auth/google/callback"},
                {mistGoogleUsers, [{"test@example.test", "test-user"}]},
                {mistGoogleTokenUrl, undefined},
                {mistGoogleProxy, false}],
    Old = [{Key, application:get_env(eTodo, Key)} || {Key, _} <- Settings],
    [application:set_env(eTodo, Key, Value) || {Key, Value} <- Settings],
    Database = spawn_link(fun database/0),
    true = register(eTodoDB, Database),
    {ok, Auth} = eTodoMistAuth:start_link(),
    Dir = filename:join(
            "/tmp", "etodo-web-notification-" ++
                integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_dir(filename:join(Dir, "server_cert.pem")),
    ok = eTodoCerts:ensure(Dir),
    {ok, Socket} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, {_, Port}} = inet:sockname(Socket),
    ok = gen_tcp:close(Socket),
    Cert = list_to_binary(filename:join(Dir, "server_cert.pem")),
    Key = list_to_binary(filename:join(Dir, "server_key.pem")),
    Server = spawn_link(
               fun() -> etodo_web:start(Port, <<"127.0.0.1">>, Cert, Key) end),
    ok = wait_ready(Port, 50),
    {Server, Auth, Database, Dir, Port, Old}.

check_google_destination(Port) ->
    {200, ReturnHeaders, _} = web_get(Port, "/messages?kind=reminders", []),
    ReturnName = "etodo_messages_" ++ integer_to_list(Port),
    ReturnCookie = cookie_pair(ReturnName, ReturnHeaders),
    {303, StartHeaders, _} = web_get(Port, "/auth/google", []),
    OidcName = "etodo_oidc_" ++ integer_to_list(Port),
    OidcCookie = cookie_pair(OidcName, StartHeaders),
    #{query := Query} = uri_string:parse(
                         proplists:get_value("location", StartHeaders)),
    Fields = uri_string:dissect_query(Query),
    Claims = #{<<"iss">> => <<"https://accounts.google.com">>,
               <<"aud">> => <<"test-client">>,
               <<"exp">> => erlang:system_time(second) + 60,
               <<"nonce">> => list_to_binary(proplists:get_value("nonce", Fields)),
               <<"email">> => <<"test@example.test">>,
               <<"email_verified">> => true},
    Payload = base64:encode(iolist_to_binary(json:encode(Claims)),
                            #{mode => urlsafe, padding => false}),
    Token = <<"e30.", Payload/binary, ".test-signature">>,
    {Listener, TokenServer, TokenUrl} = token_server(Token),
    application:set_env(eTodo, mistGoogleTokenUrl, TokenUrl),
    try
        State = proplists:get_value("state", Fields),
        {200, Headers, Page} = web_get(
                                 Port,
                                 "/auth/google/callback?code=test-code&state=" ++
                                     uri_string:quote(State),
                                 [{"cookie", OidcCookie ++ "; " ++ ReturnCookie}]),
        ?assertNotEqual(
           nomatch,
           binary:match(Page, <<"content=\"0;url=/messages?kind=reminders\"">>)),
        ?assertNotEqual(nomatch,
                        string:find(cookie_header(ReturnName, Headers),
                                    "Max-Age=0")),
        Session = cookie_pair("etodo_session_" ++ integer_to_list(Port), Headers),
        {200, _, Messages} = web_get(
                               Port, "/messages?kind=reminders",
                               [{"cookie", Session}]),
        ?assertNotEqual(
           nomatch, binary:match(Messages, <<"data-selected=\"reminders\"">>))
    after
        gen_tcp:close(Listener),
        exit(TokenServer, shutdown)
    end.

token_server(Token) ->
    {ok, Listener} = gen_tcp:listen(
                       0, [binary, {packet, http_bin}, {active, false},
                           {ip, {127, 0, 0, 1}}]),
    {ok, {_, Port}} = inet:sockname(Listener),
    Server = spawn(
               fun() ->
                       {ok, Socket} = gen_tcp:accept(Listener, 5000),
                       {ok, {http_request, 'POST', _, _}} =
                           gen_tcp:recv(Socket, 0, 5000),
                       Length = request_length(Socket, 0),
                       ok = inet:setopts(Socket, [{packet, raw}]),
                       {ok, _Body} = gen_tcp:recv(Socket, Length, 5000),
                       Response = iolist_to_binary(
                                    json:encode(#{<<"id_token">> => Token})),
                       ok = gen_tcp:send(
                              Socket,
                              ["HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n",
                               "Connection: close\r\nContent-Length: ",
                               integer_to_list(byte_size(Response)),
                               "\r\n\r\n", Response]),
                       gen_tcp:close(Socket)
               end),
    {Listener, Server, "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/token"}.

request_length(Socket, Length) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            request_length(Socket, binary_to_integer(Value));
        {ok, {http_header, _, _, _, _}} ->
            request_length(Socket, Length);
        {ok, http_eoh} ->
            Length
    end.

wait_ready(_Port, 0) ->
    error(notification_web_server_not_ready);
wait_ready(Port, Attempts) ->
    case ssl:connect("127.0.0.1", Port, [{verify, verify_none}], 100) of
        {ok, Socket} ->
            ssl:close(Socket);
        {error, _} ->
            timer:sleep(20),
            wait_ready(Port, Attempts - 1)
    end.

database() ->
    receive
        {'$gen_call', From, {readUserCfg, "test-user"}} ->
            gen_server:reply(From, #userCfg{userName = "test-user",
                                           webPassword = "test-password"}),
            database();
        {'$gen_call', From, {getMessageList, "test-user", _, _}} ->
            gen_server:reply(From, []),
            database();
        {'$gen_call', From, getUsers} ->
            gen_server:reply(From, []),
            database();
        stop ->
            ok
    end.

credentials() ->
    "username=test-user&password=test-password".

login_location(Port, Headers) ->
    {303, ResponseHeaders, _} = web_post(Port, "/login", credentials(), Headers),
    proplists:get_value("location", ResponseHeaders).

web_get(Port, Path, Headers) ->
    web_request(get, {url(Port, Path), Headers}).

web_post(Port, Path, Body, Headers) ->
    web_request(post, {url(Port, Path), Headers,
                       "application/x-www-form-urlencoded", Body}).

web_request(Method, Request) ->
    {ok, {{_, Status, _}, Headers, Body}} =
        httpc:request(Method, Request,
                      [{ssl, [{verify, verify_none}]},
                       {autoredirect, false}, {timeout, 5000}],
                      [{body_format, binary}], etodo_notification_test),
    {Status, Headers, Body}.

url(Port, Path) ->
    "https://127.0.0.1:" ++ integer_to_list(Port) ++ Path.

cookie_pair(Name, Headers) ->
    hd(string:split(cookie_header(Name, Headers), ";", all)).

cookie_header(Name, Headers) ->
    [Cookie] = [Value || {"set-cookie", Value} <- Headers,
                         lists:prefix(Name ++ "=", Value)],
    Cookie.
