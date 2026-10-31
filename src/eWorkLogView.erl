-module(eWorkLogView).

-behaviour(gen_server).

-export([new/1, load/3, load/4, load/5, document/2, document/3, navigation/1,
         allowed_url/2, page/2, page/3, report_page/4,
         desktop_report_page/7]).
-export([start_link/0, listener_started/2, bootstrap/1, valid_host/1,
         desktop_page/5, revoke/0]).
-export([load_messages/4, desktop_messages_page/7, messages_page/2,
         messages_summary/2, desktop_messages_fragment/3,
         desktop_message_action/4, desktop_user/1,
         avatar/2, desktop_avatar/2, theme_attributes/1, legacy_link/1]).
-export([markdown_html/1, markdown_inline_html/1, markdown_summary_html/2,
         markdown_token/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("eTodo/include/eTodo.hrl").

-record(state, {server, port, key, tickets = #{}, sessions = #{}}).

%% Desktop sessions are renewed while the page uses them.
-define(sessionSeconds, 28800).
-define(maxMessageLength, 4000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

listener_started(Owner, Port) ->
    Owner ! {desktop_port, Port},
    ok.

init([]) ->
    Owner = self(),
    Server = spawn_link(fun() -> etodo_web:start_work_log(Owner) end),
    receive
        {desktop_port, Port} ->
            {ok, #state{server = Server, port = Port,
                        key    = crypto:strong_rand_bytes(32)}}
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
                                     {User, Now + ?sessionSeconds}}}};
                false ->
                    {reply, {error, nil}, State#state{tickets = Tickets2}}
            end;
        error ->
            {reply, {error, nil}, State}
    end;
handle_call({session, Token}, _From, State = #state{sessions = Sessions}) ->
    Now = erlang:monotonic_time(second),
    case maps:find(Token, Sessions) of
        {ok, {User, Expires}} when Expires >= Now ->
            Renewed = Sessions#{Token => {User, Now + ?sessionSeconds}},
            {reply, {ok, User}, State#state{sessions = Renewed}};
        {ok, _Expired} ->
            {reply, {error, nil},
             State#state{sessions = maps:remove(Token, Sessions)}};
        error ->
            {reply, {error, nil}, State}
    end;
handle_call({csrf, Token}, From, State = #state{key = Key}) ->
    case handle_call({session, Token}, From, State) of
        {reply, {ok, User}, State2} ->
            {reply, {ok, User, csrf(Key, Token)}, State2};
        Reply ->
            Reply
    end.

csrf(Key, Token) ->
    binary:encode_hex(crypto:mac(hmac, sha256, Key, Token)).

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
    case gen_server:call(?MODULE, {csrf, Token}) of
        {ok, User, Csrf} ->
            Theme = #{background => Background, foreground => Foreground,
                      accent => Accent},
            eMessagesView:render_desktop(
              User, eMessagesView:kind(Kind),
              eMessagesView:filter(Filter, Peers), Theme, Csrf,
              unread_counts(User));
        _ -> {error, <<"invalid_session">>}
    end.

%% Timelines and unread counts, fetched by the page when pushed to.
desktop_messages_fragment(Token, Filter, Peers) ->
    case gen_server:call(?MODULE, {session, Token}) of
        {ok, User} ->
            eMessagesView:desktop_fragment(
              User, eMessagesView:filter(Filter, Peers), unread_counts(User));
        _ -> {error, <<"invalid_session">>}
    end.

desktop_user(Token) ->
    case gen_server:call(?MODULE, {session, Token}) of
        {ok, User} -> {ok, unicode:characters_to_binary(User)};
        _          -> {error, nil}
    end.

desktop_message_action(Token, Csrf, Action, Value) ->
    case gen_server:call(?MODULE, {csrf, Token}) of
        {ok, User, Expected} ->
            Valid = byte_size(Csrf) =:= byte_size(Expected) andalso
                crypto:hash_equals(Csrf, Expected),
            case Valid of
                true  -> message_action(User, Action, Value);
                false -> {error, <<"invalid_csrf">>}
            end;
        _ -> {error, <<"invalid_session">>}
    end.

message_action(_User, <<"send">>, Text) ->
    Message = binary:replace(Text, <<"\r\n">>, <<"\n">>, [global]),
    case string:length(Message) =< ?maxMessageLength of
        true ->
            gui_call({webChatSend, unicode:characters_to_list(Message)},
                     30000);
        false ->
            {error, <<"too_long">>}
    end;
message_action(User, <<"preview">>, Text) ->
    {ok, eMessagesView:preview(User, Text)};
message_action(_User, <<"typing">>, _Value) ->
    wx_object:cast(eTodo, webTyping),
    {ok, <<>>};
message_action(_User, <<"linkfile">>, _Value) ->
    gui_call(webLinkFile, infinity);
message_action(_User, <<"viewing">>, Kind) ->
    Type = case eMessagesView:kind(Kind) of
               chat      -> msgEntry;
               reminders -> alarmEntry;
               system    -> systemEntry
           end,
    wx_object:cast(eTodo, {msgKindShown, Type}),
    {ok, <<>>};
message_action(_User, _Action, _Value) ->
    {error, <<"unknown_action">>}.

gui_call(Request, Timeout) ->
    try wx_object:call(eTodo, Request, Timeout) of
        ok                        -> {ok, <<>>};
        {error, Reason}           -> {error, Reason};
        Text when is_list(Text)   -> {ok, unicode:characters_to_binary(Text)}
    catch
        exit:_ -> {error, <<"unavailable">>}
    end.

unread_counts(User) ->
    case eTodoDB:readUserCfg(User) of
        #userCfg{unreadMsgs2 = {C1, C2, C3}} when is_integer(C1),
                                                  is_integer(C2),
                                                  is_integer(C3) ->
            #{chat => C1, reminders => C2, system => C3};
        _ ->
            #{chat => 0, reminders => 0, system => 0}
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

markdown_html(Text) when is_binary(Text) ->
    render_markdown(Text, false).

render_markdown(Text, Inline) ->
    try
        nodes_html(markdown_nodes(Text, Inline))
    catch
        Class:Reason:Stack ->
            logger:error("Could not render task Markdown (~p:~p): ~p",
                         [Class, Reason, Stack]),
            <<"<p>Markdown preview unavailable.</p>">>
    end.

markdown_nodes(Text, Inline) ->
    Html = eMd2Html:convert(Text),
    {<<"div">>, _, Nodes} =
        mochiweb_html:parse(keep_spaces(<<"<div>", Html/binary, "</div>">>)),
    clean_markdown_nodes(Nodes, Inline).

%% mochiweb_html drops whitespace-only text, e.g. between inline elements.
keep_spaces(Html) ->
    re:replace(Html, ">[ \\t]+<", ">\\&#32;<", [global, {return, binary}]).

nodes_html(Nodes) ->
    unicode:characters_to_binary(
      [case Node of
           NodeText when is_binary(NodeText) ->
               mochiweb_html:escape(NodeText);
           _ ->
               mochiweb_html:to_html(Node)
       end
       || Node <- Nodes]).

markdown_token() ->
    binary:encode_hex(crypto:strong_rand_bytes(16)).

markdown_inline_html(Text) when is_binary(Text) ->
    [FirstLine | _] = binary:split(Text, <<"\n">>),
    Line = binary:replace(FirstLine, <<"\r">>, <<>>, [global]),
    WithoutHeading = re:replace(Line, <<"^#{1,6}[ \\t]+">>, <<>>,
                                [{return, binary}]),
    render_markdown(WithoutHeading, true).

%% First line as inline Markdown, shortened by visible text so that
%% formatting is never cut in the middle of its syntax.
markdown_summary_html(Text, Max) when is_binary(Text), Max > 3 ->
    [FirstLine | _] = binary:split(string:trim(Text, leading), <<"\n">>),
    Line = binary:replace(FirstLine, <<"\r">>, <<>>, [global]),
    WithoutHeading = re:replace(Line, <<"^#{1,6}[ \\t]+">>, <<>>,
                                [{return, binary}]),
    try
        Nodes = markdown_nodes(WithoutHeading, true),
        case text_length(Nodes) =< Max of
            true ->
                nodes_html(Nodes);
            false ->
                {Kept, _} = take_text(Nodes, Max - 3),
                nodes_html(Kept ++ [<<"...">>])
        end
    catch
        Class:Reason:Stack ->
            logger:error("Could not render task Markdown (~p:~p): ~p",
                         [Class, Reason, Stack]),
            mochiweb_html:escape(string:slice(WithoutHeading, 0, Max))
    end.

text_length(Nodes) ->
    lists:sum([case Node of
                   Text when is_binary(Text) -> string:length(Text);
                   {_Tag, _Attributes, Children} -> text_length(Children);
                   _ -> 0
               end || Node <- Nodes]).

take_text([], Left) -> {[], Left};
take_text(_Nodes, 0) -> {[], 0};
take_text([Text | Rest], Left) when is_binary(Text) ->
    case string:length(Text) of
        Length when Length =< Left ->
            {More, Left2} = take_text(Rest, Left - Length),
            {[Text | More], Left2};
        _ ->
            {[string:trim(string:slice(Text, 0, Left), trailing)], 0}
    end;
take_text([{Tag, Attributes, Children} | Rest], Left) ->
    {Kept, Left2} = take_text(Children, Left),
    {More, Left3} = take_text(Rest, Left2),
    {[{Tag, Attributes, Kept} | More], Left3};
take_text([_ | Rest], Left) ->
    take_text(Rest, Left).

clean_markdown_nodes(Nodes, Inline) ->
    lists:flatmap(fun(Node) -> clean_markdown_node(Node, Inline) end, Nodes).

clean_markdown_node({<<"a">>, Attributes, Children}, Inline) ->
    case lists:keyfind(<<"href">>, 1, Attributes) of
        {_, Href} when is_binary(Href) ->
            case safe_markdown_link(Href) of
                true ->
                    [{<<"a">>,
                      [{<<"href">>, Href},
                       {<<"target">>, <<"_blank">>},
                       {<<"rel">>, <<"noopener noreferrer">>}],
                      clean_markdown_nodes(Children, Inline)}];
                false ->
                    clean_markdown_nodes(Children, Inline)
            end;
        _ ->
            clean_markdown_nodes(Children, Inline)
    end;
clean_markdown_node({<<"img">>, Attributes, _Children}, false) ->
    case {lists:keyfind(<<"src">>, 1, Attributes),
          lists:keyfind(<<"alt">>, 1, Attributes)} of
        {{_, Src}, {_, Alt}} when is_binary(Src), is_binary(Alt) ->
            case safe_markdown_image(Src) of
                true -> [{<<"img">>, [{<<"src">>, Src}, {<<"alt">>, Alt}], []}];
                false -> []
            end;
        {{_, Src}, _} when is_binary(Src) ->
            case safe_markdown_image(Src) of
                true -> [{<<"img">>, [{<<"src">>, Src}], []}];
                false -> []
            end;
        _ ->
            []
    end;
clean_markdown_node({Tag, _Attributes, Children}, Inline) ->
    case markdown_tag_allowed(Tag, Inline) of
        true -> [{Tag, [], clean_markdown_nodes(Children, Inline)}];
        false -> clean_markdown_nodes(Children, Inline)
    end;
clean_markdown_node(Text, _Inline) when is_binary(Text) ->
    [Text];
clean_markdown_node(_, _) ->
    [].

markdown_tag_allowed(Tag, true) ->
    lists:member(Tag, [<<"strong">>, <<"b">>, <<"em">>, <<"i">>,
                       <<"del">>, <<"s">>, <<"code">>, <<"sup">>, <<"sub">>]);
markdown_tag_allowed(Tag, false) ->
    lists:member(
      Tag, [<<"p">>, <<"br">>, <<"strong">>, <<"b">>, <<"em">>, <<"i">>,
            <<"del">>, <<"s">>, <<"code">>, <<"pre">>, <<"blockquote">>,
            <<"ul">>, <<"ol">>, <<"li">>, <<"h1">>, <<"h2">>, <<"h3">>,
            <<"h4">>, <<"h5">>, <<"h6">>, <<"hr">>, <<"table">>, <<"thead">>,
            <<"tbody">>, <<"tr">>, <<"th">>, <<"td">>, <<"sup">>, <<"sub">>]).

safe_markdown_link(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} when Host =/= <<>> ->
            lists:member(string:lowercase(Scheme), [<<"http">>, <<"https">>]);
        #{scheme := <<"mailto">>, path := Path} ->
            Path =/= <<>>;
        _ ->
            false
    catch
        _:_ -> false
    end.

safe_markdown_image(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} when Host =/= <<>> ->
            lists:member(string:lowercase(Scheme), [<<"http">>, <<"https">>]);
        _ ->
            false
    catch
        _:_ -> false
    end.

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
    Content = keep_spaces(unicode:characters_to_binary(Report)),
    {_, _, Rows} = mochiweb_html:parse(Content),
    [Header | Rest] = Rows,
    ColumnCount = case Header of
                      {<<"tr">>, _, Cells} -> length(Cells);
                      _ -> 1
                  end,
    %% Only the work and time logs end with a total row; keep it as a footer.
    {Entries, Footer} =
        case maps:get(report, Theme, "work-log") of
            "schedule" -> {Rest, []};
            _ -> {lists:droplast(Rest),
                  [{<<"tfoot">>, [], [clean(lists:last(Rest))]}]}
        end,
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
              {<<"tbody">>, [], Body} | Footer]},
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

%% Desktop-only action links are unwrapped into their content. The task link
%% and Markdown links work in the browser too.
browser_children({<<"a">>, Attributes, Children} = Node) ->
    Href = proplists:get_value(<<"href">>, Attributes, <<>>),
    TaskLink = proplists:get_value(<<"class">>, Attributes) =:= <<"task-open">>,
    case {TaskLink, Href} of
        {false, <<"/work-log/action/", _/binary>>} ->
            lists:flatmap(fun browser_children/1, Children);
        _ ->
            [browser_content(Node)]
    end;
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
header_form("time-log", #{lists := Lists0, list := Selected}) ->
    Lists = eTodoUtils:sortTaskLists(Lists0),
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
            case proplists:get_value(<<"class">>, Attributes) of
                <<"task-open">> ->
                    {<<"a">>, [{<<"href">>, Url},
                               {<<"class">>, <<"task-open">>},
                               {<<"title">>, <<"Open task">>},
                               {<<"aria-label">>, <<"Open task">>}],
                     children(Content)};
                _ ->
                    {<<"a">>, [{<<"href">>, Url}], children(Content)}
            end;
        blocked -> children(Content)
    end;
%% Task descriptions are rendered Markdown; sanitize it again as such.
clean({<<"span">>, [{<<"class">>, <<"etodo-md">>}], Content}) ->
    {<<"span">>, [{<<"class">>, <<"etodo-md">>}],
     clean_markdown_nodes(Content, true)};
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