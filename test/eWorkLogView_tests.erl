-module(eWorkLogView_tests).

-include_lib("eunit/include/eunit.hrl").

document_test() ->
    Task = eTodoUtils:convertUid(42),
    Time = eTodoUtils:convertUid(42, {2026, 10, 27}),
    Report = eHtml:tableTag(
               [eHtml:trTag([eHtml:tdTag("Task"), eHtml:tdTag("Tuesday")]),
                eHtml:trTag(
                  [eHtml:tdTag(eHtml:aTag([{href, Task}], "Task &amp; text")),
                   eHtml:tdTag(eHtml:aTag([{href, Time}], "01:30"))]),
                eHtml:trTag([eHtml:tdTag("Total"), eHtml:tdTag("01:30")])]),
    Theme = #{background => <<"#171717">>, foreground => <<"#EEEEEE">>,
          accent => <<"#0088FF">>},
    Page = eWorkLogView:document(Report, <<"body { margin: 0; }">>, Theme),
    lists:foreach(
      fun(Text) -> ?assertNotEqual(nomatch, binary:match(Page, Text)) end,
      [<<"<thead>">>, <<"<tfoot>">>, <<"Task &amp; text">>,
        <<"/work-log/action/task/42">>,
        <<"/work-log/action/time/42/2026/10/27">>,
       <<"default-src 'none'">>, <<"01:30">>]).

empty_document_test() ->
    Report = "<table><tr><td>Task</td></tr><tr><td>Total</td></tr></table>",
    Page = eWorkLogView:document(Report, <<>>),
    ?assertNotEqual(nomatch, binary:match(Page, <<"No work logged">>)).

browser_time_log_header_test() ->
    Report = "<table><tr><td>Task</td></tr><tr><td>Total</td></tr></table>",
    Page = eWorkLogView:document(
             Report, <<>>,
             #{report => "time-log", list => "Arbete pågår",
               lists => ["All tasks", "Arbete pågår", "<b>x</b>"]}),
    ?assertEqual(nomatch, binary:match(Page, <<"type=\"date\"">>)),
    ?assertNotEqual(nomatch, binary:match(Page, <<"name=\"list\"">>)),
    ?assertNotEqual(nomatch, binary:match(
        Page, <<"value=\"Arbete pågår\" selected"/utf8>>)),
    ?assertNotEqual(nomatch, binary:match(Page, <<"&lt;b&gt;x&lt;/b&gt;">>)),
    Schedule = eWorkLogView:document(Report, <<>>, #{report => "schedule"}),
    ?assertEqual(nomatch, binary:match(Schedule, <<"<form">>)),
    ?assertNotEqual(nomatch, binary:match(Schedule, <<"data-live=\"reports\"">>)),
    ?assertNotEqual(nomatch, binary:match(Schedule, <<"script-src 'self'">>)),
    Desktop = eWorkLogView:document(Report, <<>>,
                                    #{background => <<"#000000">>,
                                      foreground => <<"#ffffff">>,
                                      accent => <<"#0088ff">>}),
    ?assertEqual(nomatch, binary:match(Desktop, <<"<script">>)).

browser_link_with_markup_test() ->
    Time = eTodoUtils:convertUid(42, {2026, 10, 27}),
    Report = eHtml:tableTag(
               [eHtml:trTag([eHtml:tdTag("Task"), eHtml:tdTag("Tuesday")]),
                eHtml:trTag(
                  [eHtml:tdTag("Task"),
                   eHtml:tdTag(eHtml:aTag([{href, Time}],
                                          "<span>00:00</span>"))]),
                eHtml:trTag([eHtml:tdTag("Total"), eHtml:tdTag("00:00")])]),
    Page = eWorkLogView:document(Report, <<>>,
                                 #{date => {2026, 10, 27},
                                   report => "work-log"}),
    ?assertNotEqual(nomatch, binary:match(Page, <<"class=\"zero\"">>)),
    ?assertEqual(nomatch, binary:match(Page, <<"/work-log/action/">>)).

navigation_test() ->
    ?assertEqual({ok, {uid, 42}},
                 eWorkLogView:navigation("/work-log/action/task/42")),
    ?assertEqual({ok, {uidAndDate, 42, {2026, 10, 27}}},
                 eWorkLogView:navigation(
                   "/work-log/action/time/42/2026/10/27")),
    ?assertEqual({ok, {uid, 42}},
                 eWorkLogView:navigation("etodo-worklog:task/42")),
    ?assertEqual({ok, {uidAndDate, 42, {2026, 10, 27}}},
                 eWorkLogView:navigation("etodo-worklog:time/42/2026/10/27")),
    lists:foreach(
      fun(Url) -> ?assertEqual(blocked, eWorkLogView:navigation(Url)) end,
      ["https://example.org", "file:///etc/passwd", "javascript:alert(1)",
       "etodo-worklog:task/no", "etodo-worklog:task/42?extra=1",
       "etodo-worklog:time/42/2026/2/30"]).

    browser_report_test() ->
      Task = eTodoUtils:convertUid(42),
      Report = eHtml:tableTag(
             [eHtml:trTag([eHtml:tdTag("Task")]),
            eHtml:trTag([eHtml:tdTag(eHtml:aTag([{href, Task}], "Task"))]),
            eHtml:trTag([eHtml:tdTag("Total")])]),
      Page = eWorkLogView:document(Report, <<>>),
      ?assertNotEqual(nomatch, binary:match(Page, <<"Main navigation">>)),
      ?assertNotEqual(nomatch, binary:match(Page, <<"type=\"date\"">>)),
      ?assertEqual(nomatch, binary:match(Page, <<"etodo-worklog:">>)).

    native_theme_test() ->
      Report = "<table><tr><td>Task</td></tr><tr><td>Total</td></tr></table>",
      Theme = #{background => <<"#171717">>, foreground => <<"#EEEEEE">>,
            accent => <<"#0088FF">>},
      Page = eWorkLogView:document(Report, <<>>, Theme),
      ?assertNotEqual(nomatch, binary:match(Page, <<"data-bs-theme=\"dark\"">>)),
      ?assertNotEqual(nomatch, binary:match(Page, <<"--background:#171717">>)),
      ?assertNotEqual(nomatch, binary:match(Page, <<"table-striped">>)),
      ?assertNotEqual(nomatch, binary:match(Page, <<"/assets/bootstrap.min.css">>)),
      Unsafe = Theme#{accent => <<"red;background:url(https://evil.example)">>},
      SafePage = eWorkLogView:document(Report, <<>>, Unsafe),
      ?assertEqual(nomatch, binary:match(SafePage, <<"evil.example">>)),
      ?assertEqual(nomatch, binary:match(SafePage, <<"data-embedded">>)).

    local_navigation_test() ->
      ?assert(eWorkLogView:allowed_url(
            "http://127.0.0.1:12345/work-log?date=2026-10-27", 12345)),
      ?assert(eWorkLogView:allowed_url(
        "http://127.0.0.1:12345/time-log?list=All%20tasks", 12345)),
      ?assert(eWorkLogView:allowed_url(
        "http://127.0.0.1:12345/schedule", 12345)),
      ?assert(eWorkLogView:allowed_url(
            "http://127.0.0.1:12345/desktop/ticket", 12345)),
      lists:foreach(
        fun(Url) -> ?assertNot(eWorkLogView:allowed_url(Url, 12345)) end,
        ["http://127.0.0.1:12346/work-log", "http://evil.example:12345/work-log",
         "http://user@127.0.0.1:12345/work-log", "file:///etc/passwd",
         "http://127.0.0.1:12345/settings", "javascript:alert(1)"]).

    desktop_session_test_() ->
      {timeout, 30, fun desktop_session/0}.

    desktop_session() ->
      {Server, Database} = start_fixture(),
      try
        check_desktop_session(Server)
      after
        gen_server:stop(Server),
        exit(Database, kill),
        inets:stop(httpc, worklog_test)
      end.

    start_fixture() ->
      GleamPaths = filelib:wildcard("web/backend/build/dev/erlang/*/ebin"),
      ok = code:add_paths(GleamPaths),
      {ok, _} = application:ensure_all_started(inets),
      {ok, _} = application:ensure_all_started(crypto),
      {ok, _} = application:ensure_all_started(etodo_web),
      {ok, _} = inets:start(httpc, [{profile, worklog_test}]),
      Database = spawn(fun database_fixture/0),
      true = register(eTodoDB, Database),
      {ok, Server} = eWorkLogView:start_link(),
      {Server, Database}.

    check_desktop_session(Server) ->
        Port = gen_server:call(eWorkLogView, port),
        ?assertMatch({401, _, _}, desktop_get(Port, "/work-log", [])),
        ?assertMatch({403, _, _}, desktop_get(
                 Port, "/work-log", [{"host", "evil.example"}])),
        {Port, Ticket} = gen_server:call(eWorkLogView, {ticket, "test-user"}),
        Path = "/desktop/" ++ binary_to_list(Ticket),
        {303, Headers, _} = desktop_get(Port, Path, []),
        CookieHeader = desktop_cookie_header(Headers),
        ?assertNotEqual(nomatch, string:find(CookieHeader, "HttpOnly")),
        ?assertEqual(nomatch, string:find(CookieHeader, "Max-Age")),
        Legacy = [H || {"set-cookie", H} <- Headers,
                       lists:prefix("etodo_worklog=", H)],
        ?assertEqual(2, length(Legacy)),
        ?assert(lists:all(fun(H) -> string:find(H, "Max-Age=0") =/= nomatch end,
                          Legacy)),
        Cookie = hd(string:split(CookieHeader, ";", all)),
        ?assertMatch({401, _, _}, desktop_get(Port, Path, [])),
        ReportPath = "/work-log?date=2026-10-27&background=%23171717"
          "&foreground=%23EEEEEE&accent=%230088FF",
        {200, ReportHeaders, Report} =
          desktop_get(Port, ReportPath, [{"cookie", Cookie}]),
        ?assertEqual("text/html; charset=utf-8",
               proplists:get_value("content-type", ReportHeaders)),
        Policy = proplists:get_value("content-security-policy", ReportHeaders),
        ?assertNotEqual(nomatch,
            string:find(Policy, "style-src 'self' 'unsafe-inline'")),
        ?assertNotEqual(nomatch, string:find(Policy, "script-src 'none'")),
        ?assertNotEqual(nomatch, string:find(Report, "data-bs-theme=\"dark\"")),
        ?assertNotEqual(nomatch, string:find(Report, "Peer connection testing")),
        ?assertNotEqual(nomatch,
            string:find(Report, "/work-log/action/time/42/")),
        ?assertNotEqual(nomatch,
          string:find(Report, "/work-log/action/time/44/")),
        {Port, SecondTicket} =
          gen_server:call(eWorkLogView, {ticket, "test-user"}),
        {303, SecondHeaders, _} = desktop_get(
                       Port, "/desktop/work-log/" ++
                         binary_to_list(SecondTicket), []),
        SecondCookie = hd(string:split(desktop_cookie_header(SecondHeaders),
                                       ";", all)),
        StaleFirst = "etodo_desktop=" ++ lists:duplicate(64, $0) ++ "; "
            ++ SecondCookie,
        ?assertMatch({200, _, _}, desktop_get(
                 Port, ReportPath, [{"cookie", StaleFirst}])),
        ?assertMatch({200, _, _}, desktop_get(
                 Port, ReportPath, [{"cookie", Cookie}])),
        {200, _, TimeReport} = desktop_get(
          Port,
          "/time-log?list=All%20tasks&background=%23171717"
            "&foreground=%23EEEEEE&accent=%230088FF",
          [{"cookie", SecondCookie}]),
        ?assertNotEqual(nomatch, string:find(TimeReport, "Time estimate(h)")),
        {200, _, ScheduleReport} = desktop_get(
          Port, "/schedule?list=All%20tasks&background=%23171717"
            "&foreground=%23EEEEEE&accent=%230088FF",
          [{"cookie", SecondCookie}]),
        ?assertNotEqual(nomatch, string:find(ScheduleReport, "Next alarm")),
        ?assertMatch({400, _, _}, desktop_get(
                 Port, "/work-log?date=invalid", [{"cookie", Cookie}])),
        check_desktop_messages(Port),
        %% Browser routes pass the default list as an Erlang string.
        {Port, ListTicket} =
          gen_server:call(eWorkLogView, {ticket, "test-user"}),
        {ok, ListToken} = eWorkLogView:bootstrap(ListTicket),
        ?assertMatch({ok, _}, eWorkLogView:desktop_report_page(
                                ListToken, "time-log", <<>>, "All tasks",
                                <<>>, <<>>, <<>>)),
        eWorkLogView:revoke(),
        Port = gen_server:call(eWorkLogView, port),
        ?assertMatch({401, _, _}, desktop_get(
                 Port, "/work-log", [{"cookie", Cookie}])),
        expired_ticket(Server).

    desktop_cookie_header(Headers) ->
      [Header] = [H || {"set-cookie", H} <- Headers,
                       lists:prefix("etodo_desktop=", H)],
      Header.

    check_desktop_messages(Port) ->
      {Port, Ticket} = gen_server:call(eWorkLogView, {ticket, "test-user"}),
      Query = "?kind=chat&filter=peers&peers=Ann%0ABob"
        "&background=%23171717&foreground=%23EEEEEE&accent=%230088FF",
      {303, Headers, _} = desktop_get(
        Port, "/desktop/messages/" ++ binary_to_list(Ticket) ++ Query, []),
      Location = proplists:get_value("location", Headers),
      ?assert(lists:prefix("/messages?", Location)),
      ?assert(lists:suffix("#end", Location)),
      ?assertNotEqual(nomatch, string:find(Location, "peers=Ann%0ABob")),
      Cookie = hd(string:split(desktop_cookie_header(Headers), ";", all)),
      Path = hd(string:split(Location, "#")),
      {200, PageHeaders, Page} =
        desktop_get(Port, Path, [{"cookie", Cookie}]),
      Policy = proplists:get_value("content-security-policy", PageHeaders),
      ?assertNotEqual(nomatch, string:find(Policy, "img-src 'self'")),
      ?assertNotEqual(nomatch, string:find(Page, "class=\"msg received\"")),
      ?assertNotEqual(nomatch, string:find(Page, "Fixture hello")),
      ?assertEqual(nomatch, string:find(Page, "<script")),
      ?assertMatch({401, _, _}, desktop_get(Port, Path, [])),
      {Status, _, _} = desktop_get(Port, "/avatar?user=..%2F..%2Fsecret",
                                   [{"cookie", Cookie}]),
      ?assertNotEqual(200, Status),
      {NoCookie, _, _} = desktop_get(Port, "/avatar?user=Ann", []),
      ?assertNotEqual(200, NoCookie).

    database_fixture() ->
      receive
        {'$gen_call', From, {getLoggedWork, _User, {_Year, _Month, Day}}} ->
            Work = [{"42", 1 + Day rem 3, 0}, {"43", 2, 30}, {44, 0, 45}],
          gen_server:reply(From, Work),
          database_fixture();
        {'$gen_call', From, {getWorkDesc, Uid}} ->
          Description = case Uid of
                    "42" -> "Peer connection testing";
                    "43" -> "Client appearance & accessibility";
                      Value when Value =:= "44"; Value =:= 44 ->
                        "Release preparation and documentation"
                  end,
          gen_server:reply(From, Description),
          database_fixture();
        {'$gen_call', From, {getETodos, _User, _List, _Filter,
                             _Search, _Cfg}} ->
          gen_server:reply(From, []),
          database_fixture();
        {'$gen_call', From, {getReminders, _User}} ->
          gen_server:reply(From, []),
          database_fixture();
        {'$gen_call', From, {getTodos, _User, _List}} ->
          gen_server:reply(From, []),
          database_fixture();
        {'$gen_call', From, {getMessageList, "test-user", [msgEntry],
                             ["Ann", "Bob"]}} ->
          Html = <<"<font face='Tahoma'><p>Fixture hello"
                   "<script>alert(1)</script></p></font>">>,
          gen_server:reply(From, [{os:timestamp(), msgEntry, "Ann",
                                   ["test-user"], Html}]),
          database_fixture()
      end.

    expired_ticket(Server) ->
      {_Port, Ticket} = gen_server:call(eWorkLogView, {ticket, "test-user"}),
      sys:replace_state(
        Server,
        fun({state, Listener, Port, Tickets, Sessions}) ->
            {state, Listener, Port,
             Tickets#{Ticket => {"test-user",
                                 erlang:monotonic_time(second) - 1}},
             Sessions}
        end),
      ?assertMatch({error, _}, eWorkLogView:bootstrap(Ticket)).

    desktop_get(Port, Path, Headers) ->
      Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ Path,
      {ok, {{_, Status, _}, ResponseHeaders, Body}} =
        httpc:request(get, {Url, Headers},
                 [{autoredirect, false}, {timeout, 5000}], [],
                 worklog_test),
      {Status, ResponseHeaders, Body}.