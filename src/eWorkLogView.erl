-module(eWorkLogView).

-behaviour(gen_server).

-export([new/1, load/3, load/4, load/5, document/2, document/3, navigation/1,
         allowed_url/2, page/2, page/3, report_page/4,
         desktop_report_page/7]).
-export([start_link/0, listener_started/2, bootstrap/1, valid_host/1,
         desktop_page/5, revoke/0]).
-export([load_messages/4, desktop_messages_page/7, messages_page/2,
         messages_summary/2,
         avatar/2, desktop_avatar/2, theme_attributes/1, legacy_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("eTodo/include/eTodo.hrl").

-record(state, {server, port, tickets = #{}, sessions = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

listener_started(Owner, Port) ->
    Owner ! {desktop_port, Port},
    ok.

init([]) ->
    Owner = self(),
    Server = spawn_link(fun() -> etodo_web:start_work_log(Owner) end),
    receive
        {desktop_port, Port} -> {ok, #state{server = Server, port = Port}}
    after 10000 ->
        exit(Server, shutdown),
        {stop, desktop_listener_timeout}
    end.

bootstrap(Ticket) -> gen_server:call(?MODULE, {bootstrap, Ticket}).

valid_host(Host) ->
    Port = gen_server:call(?MODULE, port),
    Host =:= iolist_to_binary(["127.0.0.1:", integer_to_list(Port)]).

revoke() -> gen_server:cast(?MODULE, revoke).

handle_call(port, _From, State = #state{port = Port}) ->
    {reply, Port, State};
handle_call({ticket, User}, _From,
            State = #state{port = Port, tickets = Tickets}) ->
    Ticket = binary:encode_hex(crypto:strong_rand_bytes(32)),
    Expires = erlang:monotonic_time(second) + 60,
    {reply, {Port, Ticket},
     State#state{tickets = Tickets#{Ticket => {User, Expires}}}};
handle_call({bootstrap, Ticket}, _From,
            State = #state{tickets = Tickets, sessions = Sessions}) ->
    case maps:take(Ticket, Tickets) of
        {{User, Expires}, Tickets2} ->
            case erlang:monotonic_time(second) =< Expires of
                true ->
                    Token = binary:encode_hex(crypto:strong_rand_bytes(32)),
                    Now = erlang:monotonic_time(second),
                    Live = maps:filter(fun(_, {_, Until}) -> Until >= Now end,
                                       Sessions),
                    {reply, {ok, Token},
                     State#state{tickets = Tickets2,
                                 sessions = Live#{Token =>
                                     {User, Now + 28800}}}};
                false ->
                    {reply, {error, nil}, State#state{tickets = Tickets2}}
            end;
        error ->
            {reply, {error, nil}, State}
    end;
handle_call({session, Token}, _From, State = #state{sessions = Sessions}) ->
    case maps:find(Token, Sessions) of
        {ok, {User, Expires}} ->
            case Expires >= erlang:monotonic_time(second) of
                true -> {reply, {ok, User}, State};
                false ->
                    {reply, {error, nil},
                     State#state{sessions = maps:remove(Token, Sessions)}}
            end;
        error ->
            {reply, {error, nil}, State}
    end.

handle_cast(revoke, State) ->
    {noreply, State#state{tickets = #{}, sessions = #{}}}.

handle_info(_Message, State) -> {noreply, State}.

terminate(_Reason, #state{server = Server}) ->
    exit(Server, shutdown),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

desktop_page(Token, Date, Background, Foreground, Accent) ->
    desktop_report_page(Token, "work-log", Date, ?defTaskList,
                        Background, Foreground, Accent).

desktop_report_page(Token, Report, Date, List, Background, Foreground, Accent) ->
    case gen_server:call(?MODULE, {session, Token}) of
        {ok, User} ->
            ReportName = report_name(Report),
            Theme = #{background => Background, foreground => Foreground,
                      accent => Accent},
            render_report(User, ReportName, Date, List, Theme);
        _ -> {error, <<"invalid_session">>}
    end.

desktop_messages_page(Token, Kind, Filter, Peers,
                      Background, Foreground, Accent) ->
    case gen_server:call(?MODULE, {session, Token}) of
        {ok, User} ->
            Theme = #{background => Background, foreground => Foreground,
                      accent => Accent},
            eMessagesView:render(User, eMessagesView:kind(Kind),
                                 eMessagesView:filter(Filter, Peers), Theme);
        _ -> {error, <<"invalid_session">>}
    end.

messages_page(Token, Kind) ->
    case eTodoMistAuth:status_form(Token) of
        {ok, {Csrf, IsOwner}} ->
            {ok, User} = eTodoMistAuth:session_user(Token),
            Composer = case IsOwner of
                           true  -> #{csrf => Csrf};
                           false -> undefined
                       end,
            %% Reading in the browser clears the desktop unread counters.
            IsOwner andalso gen_server:cast(eTodo, webMessagesRead),
            eMessagesView:render_browser(User, eMessagesView:kind(Kind),
                                         Composer);
        _ -> {error, <<"invalid_session">>}
    end.

%% Polled by the web GUI to reload messages and badge the Messages button.
messages_summary(Token, SinceText) ->
    case eTodoMistAuth:session_user(Token) of
        {ok, User} ->
            Since = try binary_to_integer(SinceText) of
                        Value when Value >= 0 -> Value;
                        _ -> 0
                    catch
                        error:badarg -> 0
                    end,
            Summary = eTodoDB:getMessageSummary(
                        unicode:characters_to_list(User), Since),
            Kinds = [{<<"chat">>, msgEntry}, {<<"reminders">>, alarmEntry},
                     {<<"system">>, systemEntry}],
            Json = maps:from_list(
                     [{Name, #{<<"latest">> => Latest, <<"new">> => New}}
                      || {Name, Type} <- Kinds,
                         {Latest, New} <- [maps:get(Type, Summary, {0, 0})]]),
            {ok, jsx:encode(Json)};
        _ -> {error, <<"invalid_session">>}
    end.

avatar(Token, Name) ->
    case eTodoMistAuth:session_user(Token) of
        {ok, _User} -> eMessagesView:avatar(Name);
        _ -> {error, <<"invalid_session">>}
    end.

desktop_avatar(Tokens, Name) ->
    Valid = fun(Token) ->
                    element(1, gen_server:call(?MODULE, {session, Token}))
                        =:= ok
            end,
    case lists:any(Valid, Tokens) of
        true  -> eMessagesView:avatar(Name);
        false -> {error, <<"invalid_session">>}
    end.

page(Token, DateText) ->
    page(Token, DateText, #{}).

page(Token, DateText, Theme) ->
    case eTodoMistAuth:session_user(Token) of
        {ok, User} ->
            render_page(User, DateText, Theme);
        _ -> {error, <<"invalid_session">>}
    end.

report_page(Token, Report, DateText, List) ->
    case eTodoMistAuth:session_user(Token) of
        {ok, User} ->
            Lists = user_lists(User),
            Wanted = unicode:characters_to_list(List),
            ListName = case lists:member(Wanted, Lists) of
                           true  -> Wanted;
                           false -> ?defTaskList
                       end,
            render_report(User, report_name(Report), DateText, ListName,
                          #{lists => Lists, list => ListName});
        _ -> {error, <<"invalid_session">>}
    end.

report_name(Report) when is_binary(Report) -> binary_to_list(Report);
report_name(Report) when is_list(Report) -> Report.

render_page(User, DateText, Theme) ->
    render_report(User, "work-log", DateText, ?defTaskList, Theme).

render_report(User, "work-log", DateText, _List, Theme) ->
    case parse_date(DateText) of
        {ok, Date} ->
            try
                Report = eHtml:makeWorkLogReport(
                           unicode:characters_to_list(User), Date),
                render_document(Report, Theme#{date => Date,
                                               report => "work-log"})
            catch
                error:Reason -> report_error("work-log", Reason)
            end;
        error -> {error, <<"invalid_date">>}
    end;
render_report(User, "time-log", _Date, List, Theme) ->
    try
        Rows = ets:new(eRowsTab, [set, protected, {keypos, 1}]),
        try
            ListName = parse_list(List),
            ETodos = eTodoDB:getETodos(unicode:characters_to_list(User),
                                       ListName, [], "", undefined),
            eRows:replace(ETodos, Rows),
            Report = eHtml:makeTimeLogReport(User, Rows,
                                              ListName =:= ?defTaskList),
            render_document(Report, Theme#{report => "time-log"})
        after
            ets:delete(Rows)
        end
    catch
        error:Reason -> report_error("time-log", Reason)
    end;
render_report(User, "schedule", _Date, _List, Theme) ->
    try
        Report = eHtml:makeScheduleReport(unicode:characters_to_list(User)),
        render_document(Report, Theme#{report => "schedule"})
    catch
        error:Reason -> report_error("schedule", Reason)
    end;
render_report(_User, _Report, _Date, _List, _Theme) ->
    {error, <<"Unknown report">>}.

report_error(Report, Reason) ->
    logger:error("Could not render ~s report: ~p", [Report, Reason]),
    {error, <<"Could not load report">>}.

parse_list(List) when is_binary(List), byte_size(List) =< 256 ->
    unicode:characters_to_list(List);
parse_list(List) when is_list(List), length(List) =< 256 -> List;
parse_list(_) -> erlang:error(invalid_list).

render_document(Report, Theme) ->
    try
        Path = filename:join([code:priv_dir(eTodo), "css", "worklog.css"]),
        {ok, Css} = file:read_file(Path),
        {ok, document(Report, Css, Theme)}
    catch
        error:_ -> {error, <<"Could not render report">>}
    end.

parse_date(<<>>) -> {ok, element(1, calendar:local_time())};
parse_date(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary>>) ->
    Date = {binary_to_integer(Year), binary_to_integer(Month),
            binary_to_integer(Day)},
    case calendar:valid_date(Date) of
        true -> {ok, Date};
        false -> error
    end;
parse_date(_) -> error.

new(Parent) ->
    View = wxWebView:new(Parent, ?wxID_ANY, [{style, ?wxBORDER_NONE}]),
    Owner = self(),
    Callback = fun(#wx{event = #wxWebView{url = Url}}, EventObject) ->
                       navigate(Owner, Url, EventObject)
               end,
    Failed = fun(#wx{event = #wxWebView{url = "about:blank"}}, _Event) ->
                     ok;
                (#wx{event = #wxWebView{url = Url, string = Message,
                                        int = Error}}, _Event) ->
                     logger:error("Report WebView failed (~p): ~ts: ~ts",
                                  [Error, Url, Message])
             end,
    wxWebView:connect(View, webview_navigating, [{callback, Callback}]),
    wxWebView:connect(View, webview_newwindow, [{callback, Callback}]),
    wxWebView:connect(View, webview_error, [{callback, Failed}]),
    wxWebView:enableContextMenu(View, [{enable, false}]),
    wxWebView:enableHistory(View, [{enable, false}]),
    View.

load(View, User, {Year, Month, Day}) ->
    load(View, User, "work-log", {Year, Month, Day}, ?defTaskList).

load(View, User, Report, List) ->
    load(View, User, Report, element(1, calendar:local_time()), List).

load(View, User, Report, {Year, Month, Day}, List) ->
    Date = lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B",
                                       [Year, Month, Day])),
    load_report(View, User, Report, [{"date", Date}, {"list", List}]).

%% Filter is the msgCfg of the GUI: {true, Peers} shows only chats with Peers.
load_messages(View, User, Kind, Filter) ->
    {Mode, Peers} = case Filter of
                        {true, Users} -> {"peers", lists:join("\n", Users)};
                        _             -> {"all", ""}
                    end,
    load_report(View, User, "messages",
                [{"kind", Kind}, {"filter", Mode},
                 {"peers", unicode:characters_to_list(Peers)}]).

load_report(View, User, Report, Params) ->
    {Port, Ticket} = gen_server:call(?MODULE, {ticket, User}),
    Background = wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOW),
    Foreground = wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOWTEXT),
    Accent     = wxSystemSettings:getColour(?wxSYS_COLOUR_HOTLIGHT),
    Query = uri_string:compose_query(
              Params ++
              [{"background", color(Background)},
               {"foreground", color(Foreground)}, {"accent", color(Accent)}]),
    Url = lists:flatten(["http://127.0.0.1:", integer_to_list(Port),
                         "/desktop/", Report, "/", binary_to_list(Ticket),
                         "?", Query]),
    wxWebView:loadURL(View, Url).

color({Red, Green, Blue, _Alpha}) ->
    lists:flatten(io_lib:format("#~2.16.0B~2.16.0B~2.16.0B",
                                [Red, Green, Blue])).

navigate(_Owner, "about:blank", Event) ->
    wxWebViewEvent:allow(Event);
navigate(Owner, Url, Event) ->
    Port = gen_server:call(?MODULE, port),
    case action_navigation(Url, Port) of
        {ok, Action} -> dispatch_action(Owner, Action, Event);
        blocked ->
            case allowed_url(Url, Port) of
                true -> wxWebViewEvent:allow(Event);
                false ->
                    wxWebViewEvent:veto(Event),
                    open_external(Url)
            end
    end.

open_external(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme} when Scheme =:= "http"; Scheme =:= "https";
                                 Scheme =:= "mailto" ->
            wx_misc:launchDefaultBrowser(Url);
        _ -> false
    catch
        error:_ -> false
    end.

dispatch_action(Owner, menu, Event) ->
    wxWebViewEvent:veto(Event),
    wx_object:cast(Owner, messageMenu);
dispatch_action(Owner, Action, Event) ->
    wxWebViewEvent:veto(Event),
    Shift = wx_misc:getKeyState(?WXK_SHIFT),
    Ctrl  = wx_misc:getKeyState(?WXK_CONTROL),
    wx_object:cast(Owner, {workLogAction, Action, Shift, Ctrl}).

action_navigation(Url, Port) ->
    case navigation(Url) of
        {ok, _Action} = Result -> Result;
        blocked -> local_action_navigation(Url, Port)
    end.

local_action_navigation(Url, Port) ->
    try uri_string:parse(Url) of
        #{scheme := "http", host := "127.0.0.1", port := Port,
          path := "/work-log/action/task/" ++ Uid} ->
            parse_action(task, [Uid]);
        #{scheme := "http", host := "127.0.0.1", port := Port,
          path := "/work-log/action/time/" ++ Path} ->
            parse_action(time, string:split(Path, "/", all));
        #{scheme := "http", host := "127.0.0.1", port := Port,
          path := "/messages/action/menu"} ->
            {ok, menu};
        _ -> blocked
    catch
        error:_ -> blocked
    end.

allowed_url(Url, Port) ->
    try uri_string:parse(Url) of
        #{scheme := "http", host := "127.0.0.1", port := Port,
          path := Path} = Parsed ->
            not maps:is_key(userinfo, Parsed) andalso
                (lists:member(Path, ["/work-log", "/time-log", "/schedule",
                                     "/messages"]) orelse
                 lists:prefix("/desktop/", Path));
        _ -> false
    catch
        error:_ -> false
    end.

navigation(Url) when is_binary(Url) ->
    navigation(binary_to_list(Url));
navigation("/work-log/action/task/" ++ Uid) ->
    parse_action(task, [Uid]);
navigation("/work-log/action/time/" ++ Path) ->
    parse_action(time, string:split(Path, "/", all));
navigation("etodo-worklog:task/" ++ Uid) ->
    parse_action(task, [Uid]);
navigation("etodo-worklog:time/" ++ Path) ->
    parse_action(time, string:split(Path, "/", all));
navigation(_) -> blocked.

parse_action(Type, Parts) ->
    try {Type, [list_to_integer(Part) || Part <- Parts]} of
        {task, [Uid]} -> {ok, {uid, Uid}};
        {time, [Uid, Year, Month, Day]} ->
            Date = {Year, Month, Day},
            case calendar:valid_date(Date) of
                true -> {ok, {uidAndDate, Uid, Date}};
                false -> blocked
            end;
        _ -> blocked
    catch
        error:_ -> blocked
    end.

document(Report, Css) ->
    document(Report, Css, #{}).

document(Report, Css, Theme) ->
    Content = unicode:characters_to_binary(Report),
    {_, _, Rows} = mochiweb_html:parse(Content),
    [Header | Rest] = Rows,
    ColumnCount = case Header of
                      {<<"tr">>, _, Cells} -> length(Cells);
                      _ -> 1
                  end,
    Total = lists:last(Rest),
    Entries = lists:droplast(Rest),
    Body = case Entries of
               [] ->
                   [{<<"tr">>, [],
                     [{<<"td">>, [{<<"colspan">>,
                                    integer_to_binary(ColumnCount)},
                                  {<<"class">>, <<"empty">>}],
                       [<<"No work logged for this period.">>]}]}];
               _ -> [clean(Node) || Node <- Entries]
           end,
    Table = {<<"table">>, [{<<"aria-label">>, <<"Logged work">>},
                            {<<"class">>,
                             <<"table table-sm table-striped table-hover">>}],
             [{<<"thead">>, [], [header(Header)]},
              {<<"tbody">>, [], Body},
              {<<"tfoot">>, [], [clean(Total)]}]},
    Attributes = theme_attributes(Theme),
    Browser = Attributes =:= "",
    Display = case Browser of
                  true  -> browser_content(Table);
                  false -> Table
              end,
    Html = mochiweb_html:to_html(Display),
    %% The web GUI reloads on pushed "reports" events; the WebView runs no JS.
    {HeaderHtml, ScriptPolicy, ScriptTag, BodyAttributes} =
        case Browser of
            true ->
                {browser_header(Theme),
                 "script-src 'self'; connect-src 'self'; ",
                 "<script src=\"/assets/etodo.js\" defer></script>",
                 " data-live=\"reports\""};
            false ->
                {<<>>, "", "", ""}
        end,
    iolist_to_binary(
      ["<!doctype html><html", Attributes,
       "><head><meta charset=\"utf-8\">",
       "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
       "<meta http-equiv=\"Content-Security-Policy\" content=\"",
       "default-src 'none'; style-src 'self' 'unsafe-inline'; ",
       "img-src data:; ", ScriptPolicy, "base-uri 'none'; form-action 'self'\">",
       "<title>", report_title(maps:get(report, Theme, "work-log")), "</title>",
       "<link rel=\"stylesheet\" href=\"/assets/bootstrap.min.css\">",
       "<style>", Css, "</style>", ScriptTag, "</head><body", BodyAttributes,
       ">", HeaderHtml, "<main>", Html, "</main></body></html>"]).

report_title("time-log") -> "Time log";
report_title("schedule") -> "Schedule";
report_title(_)          -> "Work log".

browser_content({Tag, Attributes, Children}) ->
    {Tag, Attributes, lists:flatmap(fun browser_children/1, Children)};
browser_content(Node) -> Node.

%% Desktop-only action links are unwrapped into their content.
browser_children({<<"a">>, _Attributes, Children}) ->
    lists:flatmap(fun browser_children/1, Children);
browser_children(Node) -> [browser_content(Node)].

browser_header(Theme) ->
    Report = maps:get(report, Theme, "work-log"),
    Items = [{"/", "✅", "Tasks", tasks},
             {"/work-log", "🕒", "Work log", "work-log"},
             {"/time-log", "⏳", "Time log", "time-log"},
             {"/schedule", "📅", "Schedule", "schedule"},
             {"/messages#chat", "💬", "Messages", messages}],
    ["<header class=\"report-header\"><nav class=\"main-nav\" ",
     "aria-label=\"Main navigation\">",
     [nav_item(Href, Icon, Label, current(Report, Name))
      || {Href, Icon, Label, Name} <- Items],
     "</nav>", header_form(Report, Theme), "</header>"].

nav_item(Href, Icon, Label, Current) ->
    ["<a href=\"", Href, "\" title=\"", Label, "\" aria-label=\"", Label,
     "\"", Current, ">",
     "<span class=\"nav-icon\" aria-hidden=\"true\">",
     unicode:characters_to_binary(Icon), "</span>",
     "<span class=\"nav-label\">", Label, "</span></a>"].

header_form("work-log", Theme) ->
    {Year, Month, Day} = maps:get(date, Theme,
                                element(1, calendar:local_time())),
    Date = io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]),
    ["<form method=\"get\" action=\"/work-log\">",
     "<input class=\"form-control form-control-sm\" type=\"date\" ",
     "name=\"date\" aria-label=\"Work log date\" value=\"", Date, "\">",
     load_button("Load date"), "</form>"];
header_form("time-log", #{lists := Lists, list := Selected}) ->
    Options = [{<<"option">>,
                [{<<"value">>, unicode:characters_to_binary(Name)}
                 | case Name of
                       Selected -> [{<<"selected">>, <<"selected">>}];
                       _        -> []
                   end],
                [unicode:characters_to_binary(Name)]} || Name <- Lists],
    Select = {<<"select">>, [{<<"class">>, <<"form-select form-select-sm">>},
                             {<<"name">>, <<"list">>},
                             {<<"aria-label">>, <<"Task list">>}],
              Options},
    ["<form method=\"get\" action=\"/time-log\">",
     mochiweb_html:to_html(Select), load_button("Load list"), "</form>"];
header_form(_Report, _Theme) -> [].

load_button(Label) ->
    ["<button class=\"btn btn-sm btn-outline-secondary\" type=\"submit\">",
     "<span aria-hidden=\"true\">&#8594;</span>",
     "<span class=\"visually-hidden\">", Label, "</span></button>"].

user_lists(User) ->
    UserName = unicode:characters_to_list(User),
    Configured = case eTodoDB:readUserCfg(UserName) of
                     #userCfg{lists = Lists} when is_list(Lists) -> Lists;
                     _ -> []
                 end,
    [?defTaskList | [Name || Name <- Configured, Name =/= ?defTaskList]].

current(Report, Report) -> " aria-current=\"page\"";
current(_Report, _Current) -> [].

theme_attributes(#{background := Background, foreground := Foreground,
                   accent := Accent}) ->
    case lists:all(fun valid_color/1, [Background, Foreground, Accent]) of
        true ->
            <<"#", Red:2/binary, Green:2/binary, Blue:2/binary>> = Background,
            Brightness = binary_to_integer(Red, 16) * 299 +
                binary_to_integer(Green, 16) * 587 +
                binary_to_integer(Blue, 16) * 114,
            Mode = case Brightness < 128000 of
                       true -> "dark";
                       false -> "light"
                   end,
            [" data-bs-theme=\"", Mode, "\" data-embedded=\"true\" style=\"",
             "--background:", Background, ";--foreground:", Foreground,
             ";--accent:", Accent, ";color-scheme:", Mode, "\""];
        false -> ""
    end;
theme_attributes(_) -> "".

valid_color(<<"#", Hex:6/binary>>) ->
    lists:all(fun(Char) ->
                      (Char >= $0 andalso Char =< $9) orelse
                      (Char >= $a andalso Char =< $f) orelse
                      (Char >= $A andalso Char =< $F)
              end, binary_to_list(Hex));
valid_color(_) -> false.

header({<<"tr">>, _Attributes, Cells}) ->
    {<<"tr">>, [],
     [{<<"th">>, [{<<"scope">>, <<"col">>}], children(Content)}
      || {<<"td">>, _, Content} <- Cells]}.

children(Nodes) ->
    lists:flatmap(fun(Node) ->
                          case clean(Node) of
                              Cleaned when is_list(Cleaned) -> Cleaned;
                              Cleaned -> [Cleaned]
                          end
                  end, Nodes).

clean({<<"font">>, _Attributes, Content}) -> children(Content);
clean({<<"a">>, Attributes, Content}) ->
    Href = proplists:get_value(<<"href">>, Attributes, <<>>),
    case legacy_link(Href) of
        {ok, Url} ->
            {<<"a">>, [{<<"href">>, Url}], children(Content)};
        blocked -> children(Content)
    end;
clean({<<"span">>, _Attributes, Content}) ->
    {<<"span">>, [{<<"class">>, <<"zero">>}], children(Content)};
clean({Tag, _Attributes, Content})
  when Tag =:= <<"tr">>; Tag =:= <<"td">> ->
    {Tag, [], children(Content)};
clean({_, _, Content}) -> children(Content);
clean(Text) -> Text.

legacy_link(Href) ->
    try binary_to_term(base64:decode(Href), [safe]) of
        {uid, Uid} when is_integer(Uid) ->
            {ok, iolist_to_binary(["/work-log/action/task/",
                                   integer_to_list(Uid)])};
        {uidAndDate, Uid, {Year, Month, Day}}
          when is_integer(Uid), is_integer(Year), is_integer(Month),
               is_integer(Day) ->
            Url = io_lib:format("/work-log/action/time/~B/~B/~B/~B",
                                 [Uid, Year, Month, Day]),
            {ok, iolist_to_binary(Url)};
        _ -> blocked
    catch
        error:_ -> blocked
    end.