-module(eTodoWebNotification_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

notification_destination_test_() ->
    {timeout, 30, fun notification_destination/0}.

notification_destination() ->
    Fixture = {_, _, _, _, Port, _} = start_fixture(),
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
        stop_fixture(Fixture)
    end.

task_edit_details_test_() ->
    {timeout, 30, fun task_edit_details/0}.

task_edit_details() ->
    Fixture = {_, Auth, _, _, Port, _} = start_fixture(),
    Lock = spawn(fun lock_fixture/0),
    true = register(ePeerLock, Lock),
    try
        Token = <<"task-test-token">>,
        Csrf = <<"task-test-csrf">>,
        Session = {<<"test-user">>, erlang:system_time(second) + 60, Csrf},
        sys:replace_state(Auth, fun(_) -> #{Token => Session} end),
        Cookie = "etodo_session_" ++ integer_to_list(Port) ++ "=task-test-token",
        Headers = [{"cookie", Cookie}],
        {200, InitialHeaders, InitialPage} =
            web_get(Port, "/?list=All%20tasks", Headers),
        Policy = proplists:get_value("content-security-policy", InitialHeaders),
        ?assertNotEqual(nomatch, string:find(Policy, "style-src 'self'")),
        ?assertEqual(nomatch, string:find(Policy, "'unsafe-inline'")),
        InitialTree = mochiweb_html:parse(InitialPage),
        OwnCard = find_element(InitialTree, <<"id">>, <<"task-77">>),
        SharedCard = find_element(InitialTree, <<"id">>, <<"task-78">>),
        CompleteCard = find_element(InitialTree, <<"id">>, <<"task-79">>),
        ?assertNotEqual(not_found, OwnCard),
        ?assertNotEqual(not_found, SharedCard),
        ?assertNotEqual(not_found, CompleteCard),
        ?assertNotEqual(nomatch, binary:match(node_html(OwnCard), <<"Owner">>)),
        ?assertEqual(nomatch, binary:match(node_html(OwnCard), <<"Shared with">>)),
        ?assertNotEqual(
           nomatch, binary:match(node_html(SharedCard), <<"Shared with">>)),
        ?assertNotEqual(
           nomatch, binary:match(node_html(SharedCard), <<"teammate">>)),
        ?assertEqual(not_found, find_element(OwnCard, <<"class">>,
                                             <<"etodo-progress-track">>)),
        assert_progress(SharedCard, 50),
        assert_progress(CompleteCard, 100),

        Description = "**Task**\n\nSecond paragraph",
        Comment = "First comment\n\nSecond comment",
        PreviewBody = uri_string:compose_query(
                        [{"csrf", binary_to_list(Csrf)}, {"text", Description}]),
        {200, _, Preview} = web_post(Port, "/todo/markdown-preview",
                                     PreviewBody, Headers),
        ?assertEqual(
           <<"<p><strong>Task</strong></p><p>Second paragraph</p>">>, Preview),
        Body = uri_string:compose_query(
                 [{"uid", "77"}, {"csrf", binary_to_list(Csrf)},
                  {"list", "All tasks"}, {"status", "Planning"},
                  {"priority", "Low"}, {"description", Description},
                  {"comment", Comment}, {"progress", "3"}, {"due_time", ""},
                  {"estimate", "0"}, {"remaining", "0"},
                  {"lists", "All tasks"}]),
        {303, ResponseHeaders, _} = web_post(Port, "/todo/update", Body, Headers),
        ?assertEqual("/?list=All%20tasks#task-77",
                     proplists:get_value("location", ResponseHeaders)),
        Stored = eTodoDB:getTodo(77),
        ?assertEqual(Description, Stored#todo.description),
        ?assertEqual(Comment, Stored#todo.comment),
        ?assertEqual(3, Stored#todo.progress),
        {200, _, UpdatedPage} = web_get(Port, "/?list=All%20tasks", Headers),
        UpdatedCard = find_element(mochiweb_html:parse(UpdatedPage),
             <<"id">>, <<"task-77">>),
        assert_progress(UpdatedCard, 3),
        DescriptionSection = find_element(UpdatedCard, <<"aria-label">>,
                    <<"Description">>),
        CommentSection = find_element(UpdatedCard, <<"aria-label">>,
                <<"Comment">>),
        ?assertNotEqual(not_found, DescriptionSection),
        ?assertNotEqual(not_found, CommentSection),
        ?assertNotEqual(
           nomatch,
           binary:match(node_html(DescriptionSection),
                        <<"<p><strong>Task</strong></p><p>Second paragraph</p>">>)),
        ?assertNotEqual(
           nomatch,
           binary:match(node_html(CommentSection),
                        <<"<p>First comment</p><p>Second comment</p>">>))
    after
        LockRef = monitor(process, Lock),
        Lock ! stop,
        receive {'DOWN', LockRef, process, Lock, normal} -> ok end,
        stop_fixture(Fixture)
    end.

find_element({_, Attributes, Children} = Node, Key, Value) ->
    case proplists:get_value(Key, Attributes) of
        Value -> Node;
        _ -> find_element_in_children(Children, Key, Value)
    end;
find_element(_, _Key, _Value) ->
    not_found.

find_element_in_children([], _Key, _Value) ->
    not_found;
find_element_in_children([Node | Rest], Key, Value) ->
    case find_element(Node, Key, Value) of
        not_found -> find_element_in_children(Rest, Key, Value);
        Found -> Found
    end.

node_html(Node) ->
    iolist_to_binary(mochiweb_html:to_html(Node)).

assert_progress(Card, Value) ->
    {<<"progress">>, Attributes, _} =
        find_element(Card, <<"class">>, <<"etodo-progress-track">>),
    ?assertEqual(integer_to_binary(Value),
                 proplists:get_value(<<"value">>, Attributes)),
    ?assertEqual(<<"100">>, proplists:get_value(<<"max">>, Attributes)),
    ?assertEqual(<<"Progress">>, proplists:get_value(<<"aria-label">>, Attributes)),
    ?assertEqual(undefined, proplists:get_value(<<"style">>, Attributes)),
    Label = iolist_to_binary(["Progress ", integer_to_list(Value), "%"]),
    ?assertNotEqual(nomatch, binary:match(node_html(Card), Label)).

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

stop_fixture({Server, Auth, Database, Dir, _Port, Old}) ->
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
     end || {Key, Value} <- Old],
    ok.

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
    Own = #todo{uid = 77, status = planning, priority = low,
                description = "Original task", comment = "", progress = 0,
                owner = "test-user", sharedWith = [],
                createTime = {{2026, 1, 1}, {12, 30, 0}}},
    Shared = Own#todo{uid = 78, description = "Shared task", progress = 50,
                      sharedWith = ["test-user", "teammate"]},
    Complete = Own#todo{uid = 79, description = "Complete progress",
                        progress = 100},
    database(#{77 => Own, 78 => Shared, 79 => Complete}).

database(Todos) ->
    receive
        {'$gen_call', From, {readUserCfg, "test-user"}} ->
            gen_server:reply(From, #userCfg{userName = "test-user",
                                           lists = ["All tasks"],
                                           webPassword = "test-password"}),
            database(Todos);
        {'$gen_call', From, {getMessageList, "test-user", _, _}} ->
            gen_server:reply(From, []),
            database(Todos);
        {'$gen_call', From, getUsers} ->
            gen_server:reply(From, []),
            database(Todos);
        {'$gen_call', From, {getETodos, "test-user", _, _, _, _}} ->
            gen_server:reply(
              From, [fixture_etodo(Todo) ||
                        {_Uid, Todo} <- lists:sort(maps:to_list(Todos))]),
            database(Todos);
        {'$gen_call', From, {getTime, _Uid}} ->
            gen_server:reply(From, {0, 0}),
            database(Todos);
        {'$gen_call', From, {getLists, "test-user", _Uid}} ->
            gen_server:reply(From, ["All tasks"]),
            database(Todos);
        {'$gen_call', From, {getTodo, Uid}} ->
            gen_server:reply(From, maps:get(Uid, Todos, undefined)),
            database(Todos);
        {'$gen_call', From, {updateTodo, Todo = #todo{uid = Uid}}} ->
            gen_server:reply(From, #diff{uid = Uid, diff = []}),
            database(Todos#{Uid => Todo});
        {'$gen_call', From, {saveTime, _Uid, _Estimate, _Remaining}} ->
            gen_server:reply(From, {atomic, ok}),
            database(Todos);
        {'$gen_call', From, {getReminder, "test-user", _Uid}} ->
            gen_server:reply(From, undefined),
            database(Todos);
        {'$gen_call', From, {getReminders, "test-user"}} ->
            gen_server:reply(From, []),
            database(Todos);
        {'$gen_call', From, Request} ->
            gen_server:reply(From, {error, {unexpected_fixture_request, Request}}),
            database(Todos);
        stop ->
            ok
    end.

fixture_etodo(Todo = #todo{uid = Uid, owner = Owner}) ->
    #etodo{uid = integer_to_list(Uid), uidDB = Uid,
           status = eTodoUtils:toStr(Todo#todo.status),
           statusDB = Todo#todo.status,
           priority = eTodoUtils:toStr(Todo#todo.priority),
           owner = Owner,
           sharedWith = eTodoUtils:makeStr(
                          eTodoUtils:default(Todo#todo.sharedWith, [Owner])),
           description = Todo#todo.description, comment = Todo#todo.comment,
           progress = Todo#todo.progress, dueTimeDB = Todo#todo.dueTime,
           createTimeDB = Todo#todo.createTime, doneTimeDB = Todo#todo.doneTime,
           hasSubTodo = false, lists = "All tasks", listsDB = ["All tasks"]}.

lock_fixture() ->
    receive
        {'$gen_call', From, {getLocks, _Uid, _Owner, _Users}} ->
            gen_server:reply(From, ok),
            lock_fixture();
        {'$gen_cast', {releaseLocks, _Uid, _Owner, _Users}} ->
            lock_fixture();
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
