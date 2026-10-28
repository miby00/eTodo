-module(eMessagesView).

%% Renders stored chat, reminder and system messages as a themed HTML
%% timeline. Used by the desktop WebView and by the browser web GUI.

-export([render/4, render_browser/3, render_desktop/6, desktop_fragment/3,
         preview/2, kind/1, filter/2, avatar/1, online_peers/1]).
-export([document/5, browser_document/4, browser_document/5,
         parse_legacy/1, sanitize/1, plain/1]).

-define(maxMessages, 500).
-define(groupSeconds, 300).

-define(keepTags,
        [<<"p">>, <<"br">>, <<"b">>, <<"strong">>, <<"i">>, <<"em">>,
         <<"u">>, <<"s">>, <<"del">>, <<"ins">>, <<"code">>, <<"pre">>,
         <<"kbd">>, <<"blockquote">>, <<"ul">>, <<"ol">>, <<"li">>,
         <<"h1">>, <<"h2">>, <<"h3">>, <<"h4">>, <<"h5">>, <<"h6">>,
         <<"table">>, <<"thead">>, <<"tbody">>, <<"tfoot">>, <<"tr">>,
         <<"th">>, <<"td">>, <<"hr">>, <<"sub">>, <<"sup">>, <<"mark">>,
         <<"small">>]).

-define(dropTags,
        [<<"script">>, <<"style">>, <<"head">>, <<"title">>, <<"iframe">>,
         <<"object">>, <<"embed">>, <<"svg">>, <<"math">>, <<"template">>,
         <<"noscript">>, <<"textarea">>, <<"select">>, <<"form">>,
         <<"button">>, <<"input">>, <<"meta">>, <<"link">>, <<"base">>,
         <<"frame">>, <<"frameset">>, <<"applet">>, <<"audio">>,
         <<"video">>, <<"canvas">>]).

-define(emotes,
        #{<<"crying.png">>   => <<"😭"/utf8>>,
          <<"happy.png">>    => <<"🙂"/utf8>>,
          <<"heart.png">>    => <<"❤️"/utf8>>,
          <<"lol.png">>      => <<"😂"/utf8>>,
          <<"mischief.png">> => <<"😈"/utf8>>,
          <<"sad.png">>      => <<"🙁"/utf8>>,
          <<"shocked.png">>  => <<"😮"/utf8>>,
          <<"wink.png">>     => <<"😉"/utf8>>}).

%%======================================================================
%% Request parameters
%%======================================================================
kind(<<"reminders">>) -> reminders;
kind(<<"system">>)    -> system;
kind(_)               -> chat.

filter(<<"peers">>, Peers) when is_binary(Peers) ->
    [unicode:characters_to_list(Peer)
     || Peer <- binary:split(Peers, <<"\n">>, [global, trim_all])];
filter(_, _) -> all.

%%======================================================================
%% Avatars
%%======================================================================
avatar(Name) ->
    case portrait(Name) of
        {ok, Path} ->
            case file:read_file(Path) of
                {ok, Png} -> {ok, Png};
                {error, _} -> {error, <<"not_found">>}
            end;
        error -> {error, <<"not_found">>}
    end.

portrait(Name) when is_list(Name) ->
    portrait(unicode:characters_to_binary(Name));
portrait(<<"Slack - ", SlackUser/binary>> = Name) ->
    case portrait_file(Name) of
        {ok, Path} -> {ok, Path};
        error -> portrait_file(SlackUser)
    end;
portrait(Name) when is_binary(Name) -> portrait_file(Name);
portrait(_) -> error.

portrait_file(Name) ->
    case safe_name(Name) of
        true ->
            Path = filename:join([code:priv_dir(eTodo), "Icons",
                                  <<"portrait_", Name/binary, ".png">>]),
            case filelib:is_regular(Path) of
                true -> {ok, Path};
                false -> error
            end;
        false -> error
    end.

safe_name(Name) ->
    byte_size(Name) > 0 andalso byte_size(Name) =< 128 andalso
        Name =/= <<".">> andalso Name =/= <<"..">> andalso
        binary:match(Name, [<<"/">>, <<"\\">>, <<0>>, <<"..">>]) =:= nomatch.

%%======================================================================
%% Rendering
%%======================================================================
render(User, Kind, Filter, Theme) ->
    safe_render(fun() ->
                        Records = fetch(User, Kind, Filter),
                        document(User, Kind, Records, stylesheet(), Theme)
                end).

%% The web GUI shows all kinds as swipeable panels on one page.
render_browser(User, Kind, Composer) ->
    safe_render(fun() ->
                        Panels = [{Name, fetch(User, Name, all)}
                                  || Name <- [chat, reminders, system]],
                        browser_document(User, Kind, Panels, stylesheet(),
                                         Composer)
                end).

%% The desktop client shows one kind at a time, with the composer in Chat.
render_desktop(User, Kind, Filter, Theme, Csrf, Counts) ->
    safe_render(fun() ->
                        desktop_document(User, Kind,
                                         desktop_panels(User, Filter),
                                         stylesheet(), Theme, Csrf, Counts)
                end).

desktop_fragment(User, Filter, Counts) ->
    safe_render(
      fun() ->
              Timelines =
                  [{kind_name(Kind),
                    iolist_to_binary(mochiweb_html:to_html(
                                       timeline_node(User, Kind, Records,
                                                     true)))}
                   || {Kind, Records} <- desktop_panels(User, Filter)],
              jsx:encode(maps:from_list([{<<"counts">>, Counts}
                                         | Timelines]))
      end).

%% How a message will look once sent.
preview(User, Text) ->
    Name      = unicode:characters_to_list(User),
    {Html, _} = eHtml:generateMsg(Name, Name, [],
                                  unicode:characters_to_list(Text)),
    Record    = {os:timestamp(), msgEntry, Name, [],
                 unicode:characters_to_binary(Html)},
    Message   = message(unicode:characters_to_binary(User), Record),
    iolist_to_binary(mochiweb_html:to_html(entry(Message, false, true))).

desktop_panels(User, Filter) ->
    [{chat,      fetch(User, chat, Filter)},
     {reminders, fetch(User, reminders, all)},
     {system,    fetch(User, system, all)}].

safe_render(Render) ->
    try
        {ok, Render()}
    catch
        error:Reason ->
            logger:error("Could not render messages: ~p", [Reason]),
            {error, <<"Could not load messages">>}
    end.

fetch(User, Kind, Filter) ->
    eTodoDB:getMessageList(unicode:characters_to_list(User), types(Kind),
                           Filter).

stylesheet() ->
    Path = filename:join([code:priv_dir(eTodo), "css", "messages.css"]),
    {ok, Css} = file:read_file(Path),
    Css.

types(chat)      -> [msgEntry];
types(reminders) -> [alarmEntry];
types(system)    -> [systemEntry].

document(User, Kind, Records, Css, Theme) ->
    case eWorkLogView:theme_attributes(Theme) of
        "" ->
            browser_document(User, Kind, [{Kind, Records}], Css);
        Attributes ->
            Main = {<<"main">>, [{<<"class">>, kind_name(Kind)}],
                    [timeline_node(User, Kind, Records, true),
                     {<<"div">>, [{<<"id">>, <<"end">>}], []}]},
            page(Attributes, "", false, [menu_button(), Main], Css)
    end.

browser_document(User, Selected, Panels, Css) ->
    browser_document(User, Selected, Panels, Css, undefined).

browser_document(User, Selected, Panels, Css, Composer) ->
    Sections = [{<<"section">>,
                 [{<<"id">>, kind_name(Kind)}, {<<"class">>, <<"panel">>},
                  {<<"aria-label">>, kind_label(Kind)}],
                 [{<<"div">>, [{<<"class">>, <<"panel-scroll">>}],
                   [timeline_node(User, Kind, Records, false)]}
                  | case Kind of
                        chat -> composer(User, Records, Composer);
                        _    -> []
                    end]}
                || {Kind, Records} <- Panels],
    Main = {<<"main">>, [{<<"class">>, <<"panels">>},
                         {<<"data-selected">>, kind_name(Selected)}],
            Sections},
    Header = browser_header(Selected, [Kind || {Kind, _} <- Panels]),
    page("", " data-swipe-prev=\"/\"", true, [Header, Main], Css).

desktop_document(User, Selected, Panels, Css, Theme, Csrf, Counts) ->
    Sections = [{<<"section">>,
                 [{<<"id">>, kind_name(Kind)}, {<<"class">>, <<"panel">>},
                  {<<"aria-label">>, kind_label(Kind)}],
                 [{<<"div">>, [{<<"class">>, <<"panel-scroll">>}],
                   [timeline_node(User, Kind, Records, true)]}
                  | case Kind of
                        chat -> desktop_composer(Csrf);
                        _    -> []
                    end]}
                || {Kind, Records} <- Panels],
    Main = {<<"main">>, [{<<"class">>, <<"panels">>},
                         {<<"data-selected">>, kind_name(Selected)}],
            Sections},
    Tabs = [nav_link(<<"#", (kind_name(Kind))/binary>>,
                     [kind_label(Kind), tab_count(Kind, Counts)],
                     Kind =:= Selected)
            || {Kind, _} <- Panels],
    Header = {<<"header">>, [{<<"class">>, <<"report-header desktop">>}],
              [{<<"nav">>, [{<<"class">>, <<"kinds">>},
                            {<<"aria-label">>, <<"Message type">>}], Tabs},
               menu_button()]},
    page(eWorkLogView:theme_attributes(Theme), " data-desktop=\"messages\"",
         true, [Header, Main], Css).

tab_count(Kind, Counts) ->
    Text = case maps:get(Kind, Counts, 0) of
               Count when is_integer(Count), Count > 0 ->
                   integer_to_binary(Count);
               _ -> <<>>
           end,
    {<<"span">>, [{<<"class">>, <<"tab-count">>},
                  {<<"data-kind">>, kind_name(Kind)}], [Text]}.

%% Recipients are picked in the user lists next to the view.
desktop_composer(Csrf) ->
    Smileys = [{<<":-)">>, <<"🙂"/utf8>>, <<"Happy">>},
               {<<":D">>,  <<"😂"/utf8>>, <<"LOL">>},
               {<<";-)">>, <<"😉"/utf8>>, <<"Wink">>},
               {<<":P">>,  <<"😈"/utf8>>, <<"Mischief">>},
               {<<":O">>,  <<"😮"/utf8>>, <<"Shocked">>},
               {<<":,(">>, <<"😭"/utf8>>, <<"Crying">>},
               {<<"<3">>,  <<"❤️"/utf8>>, <<"Heart">>}],
    Tools = [tool_button([{<<"data-insert">>, Code}], Emoji, Title)
             || {Code, Emoji, Title} <- Smileys] ++
        [tool_button([{<<"data-action">>, <<"linkfile">>}],
                     <<"📎"/utf8>>, <<"Link a file">>),
         tool_button([{<<"data-action">>, <<"preview">>},
                      {<<"aria-pressed">>, <<"false">>}],
                     <<"👁️"/utf8>>, <<"Preview">>)],
    [{<<"form">>, [{<<"class">>, <<"composer desktop">>},
                   {<<"method">>, <<"post">>},
                   {<<"action">>, <<"/messages/send">>}],
      [{<<"input">>, [{<<"type">>, <<"hidden">>},
                      {<<"name">>, <<"csrf">>},
                      {<<"value">>, Csrf}], []},
       {<<"div">>, [{<<"class">>, <<"composer-tools">>}], Tools},
       {<<"div">>, [{<<"class">>, <<"composer-preview">>},
                    {<<"hidden">>, <<"hidden">>}], []},
       {<<"textarea">>, [{<<"name">>, <<"value">>},
                         {<<"class">>, <<"form-control">>},
                         {<<"rows">>, <<"5">>},
                         {<<"maxlength">>, <<"4000">>},
                         {<<"placeholder">>,
                          <<"Message (Enter sends, Shift+Enter new line)">>},
                         {<<"aria-label">>, <<"Message">>}], []},
       {<<"button">>, [{<<"type">>, <<"submit">>},
                       {<<"class">>, <<"btn btn-primary">>},
                       {<<"title">>, <<"Send">>},
                       {<<"aria-label">>, <<"Send">>}],
        [<<"➤"/utf8>>]},
       {<<"p">>, [{<<"class">>, <<"composer-status">>},
                  {<<"role">>, <<"status">>}], []}]}].

tool_button(Attributes, Icon, Title) ->
    {<<"button">>, [{<<"type">>, <<"button">>}, {<<"title">>, Title},
                    {<<"aria-label">>, Title} | Attributes],
     [Icon]}.

page(HtmlAttributes, BodyAttributes, Script, Nodes, Css) ->
    {ScriptPolicy, ScriptTag} =
        case Script of
            true  -> {"script-src 'self'; connect-src 'self'; ",
                      "<script src=\"/assets/etodo.js\" defer></script>"};
            false -> {"", ""}
        end,
    iolist_to_binary(
      ["<!doctype html><html", HtmlAttributes,
       "><head><meta charset=\"utf-8\">",
       "<meta name=\"viewport\" content=\"width=device-width,"
       "initial-scale=1\">",
       "<meta http-equiv=\"Content-Security-Policy\" content=\"",
       "default-src 'none'; style-src 'self' 'unsafe-inline'; "
       "img-src 'self' data:; ", ScriptPolicy,
       "base-uri 'none'; form-action 'self'\">",
       "<title>Messages</title>",
       "<link rel=\"stylesheet\" href=\"/assets/bootstrap.min.css\">",
       "<style>", Css, "</style>", ScriptTag, "</head><body",
       BodyAttributes, ">",
       [mochiweb_html:to_html(Node) || Node <- Nodes],
       "</body></html>"]).

timeline_node(User, Kind, Records, Embedded) ->
    UserBin  = unicode:characters_to_binary(User),
    Total    = length(Records),
    Shown    = lists:nthtail(max(0, Total - ?maxMessages), Records),
    Messages = collapse([message(UserBin, Record) || Record <- Shown]),
    Items    = case timeline(Messages, Embedded) of
                   []    -> [empty(Kind)];
                   Found -> Found
               end,
    Older    = case Total - length(Shown) of
                   0 -> [];
                   Hidden ->
                       [{<<"p">>, [{<<"class">>, <<"older">>}],
                         [integer_to_binary(Hidden),
                          <<" older messages are not shown">>]}]
               end,
    {<<"div">>, [{<<"class">>, <<"timeline">>}], Older ++ Items}.

online_peers(User) ->
    UserName = unicode:characters_to_list(User),
    lists:sort([Peer || Peer <- eTodoDB:getUsers(), is_list(Peer),
                        Peer =/= UserName, ePeerCircle:isActivePeer(Peer)]).

composer(_User, _Records, undefined) -> [];
composer(User, Records, #{csrf := Csrf}) ->
    case online_peers(User) of
        [] ->
            [{<<"p">>, [{<<"class">>, <<"composer-empty">>}],
              [<<"Nobody is online right now, so messages can't be sent.">>]}];
        Peers ->
            Default = last_sender(unicode:characters_to_list(User), Records),
            Everyone = case Peers of
                           [_, _ | _] ->
                               [option(<<"All">>, <<"Everyone online">>,
                                       false)];
                           _ -> []
                       end,
            Options = Everyone ++
                [option(unicode:characters_to_binary(Peer),
                        unicode:characters_to_binary(Peer), Peer =:= Default)
                 || Peer <- Peers],
            [{<<"form">>, [{<<"class">>, <<"composer">>},
                           {<<"method">>, <<"post">>},
                           {<<"action">>, <<"/messages/send">>}],
              [{<<"input">>, [{<<"type">>, <<"hidden">>},
                              {<<"name">>, <<"csrf">>},
                              {<<"value">>, Csrf}], []},
               {<<"select">>, [{<<"name">>, <<"to">>},
                               {<<"class">>, <<"form-select form-select-sm">>},
                               {<<"aria-label">>, <<"Recipient">>}], Options},
               {<<"textarea">>, [{<<"name">>, <<"msg">>},
                                 {<<"class">>, <<"form-control">>},
                                 {<<"rows">>, <<"1">>},
                                 {<<"maxlength">>, <<"4000">>},
                                 {<<"required">>, <<"required">>},
                                 {<<"placeholder">>, <<"Message">>},
                                 {<<"aria-label">>, <<"Message">>}], []},
               {<<"button">>, [{<<"type">>, <<"submit">>},
                               {<<"class">>, <<"btn btn-primary">>},
                               {<<"title">>, <<"Send">>},
                               {<<"aria-label">>, <<"Send">>}],
                [<<"➤"/utf8>>]}]}]
    end.

option(Value, Label, true) ->
    {<<"option">>, [{<<"value">>, Value}, {<<"selected">>, <<"selected">>}],
     [Label]};
option(Value, Label, false) ->
    {<<"option">>, [{<<"value">>, Value}], [Label]}.

%% Reply goes to whoever wrote last, like the desktop client.
last_sender(User, Records) ->
    case [From || {_, _, From, _, _} <- lists:reverse(Records),
                  is_list(From), From =/= User] of
        [From | _] -> From;
        []         -> undefined
    end.

kind_name(chat)      -> <<"chat">>;
kind_name(reminders) -> <<"reminders">>;
kind_name(system)    -> <<"system">>.

kind_label(chat)      -> <<"💬 Chat"/utf8>>;
kind_label(reminders) -> <<"⏰ Reminders"/utf8>>;
kind_label(system)    -> <<"🤖 System"/utf8>>.

menu_button() ->
    {<<"a">>, [{<<"class">>, <<"menu-button">>},
               {<<"href">>, <<"/messages/action/menu">>},
               {<<"title">>, <<"Message options">>},
               {<<"aria-label">>, <<"Message options">>}],
     [<<"⋯"/utf8>>]}.

browser_header(Selected, Kinds) ->
    Nav = [{<<"/">>, <<"✅"/utf8>>, <<"Tasks">>, false},
           {<<"/work-log">>, <<"🕒"/utf8>>, <<"Work log">>, false},
           {<<"/time-log">>, <<"⏳"/utf8>>, <<"Time log">>, false},
           {<<"/schedule">>, <<"📅"/utf8>>, <<"Schedule">>, false},
           {<<"/messages#chat">>, <<"💬"/utf8>>, <<"Messages">>, true}],
    {<<"header">>, [{<<"class">>, <<"report-header">>}],
     [{<<"nav">>, [{<<"class">>, <<"main-nav">>},
                   {<<"aria-label">>, <<"Main navigation">>}],
       [main_nav_link(Href, Icon, Label, Current)
        || {Href, Icon, Label, Current} <- Nav]},
      {<<"nav">>, [{<<"class">>, <<"kinds">>},
                   {<<"aria-label">>, <<"Message type">>}],
       [nav_link(<<"#", (kind_name(Kind))/binary>>, kind_label(Kind),
                 Kind =:= Selected)
        || Kind <- Kinds]}]}.

main_nav_link(Href, Icon, Label, Current) ->
    {<<"a">>, Attributes, Content} =
        nav_link(Href,
                 [{<<"span">>, [{<<"class">>, <<"nav-icon">>},
                                {<<"aria-hidden">>, <<"true">>}], [Icon]},
                  {<<"span">>, [{<<"class">>, <<"nav-label">>}], [Label]}],
                 Current),
    {<<"a">>, Attributes ++ [{<<"title">>, Label}, {<<"aria-label">>, Label}],
     Content}.

nav_link(Href, Content, Current) when not is_list(Content) ->
    nav_link(Href, [Content], Current);
nav_link(Href, Content, true) ->
    {<<"a">>, [{<<"href">>, Href}, {<<"aria-current">>, <<"page">>}],
     Content};
nav_link(Href, Content, false) ->
    {<<"a">>, [{<<"href">>, Href}], Content}.

empty(chat) ->
    empty(<<"💬"/utf8>>, <<"No messages yet">>,
          <<"Say hi to someone, it is quiet in here.">>);
empty(reminders) ->
    empty(<<"⏰"/utf8>>, <<"No reminders">>,
          <<"Nothing to nag you about right now.">>);
empty(system) ->
    empty(<<"🌙"/utf8>>, <<"All quiet">>,
          <<"System notifications will show up here.">>).

empty(Icon, Title, Text) ->
    {<<"div">>, [{<<"class">>, <<"empty">>}],
     [{<<"div">>, [{<<"class">>, <<"empty-icon">>}], [Icon]},
      {<<"h2">>, [], [Title]},
      {<<"p">>, [], [Text]}]}.

%%======================================================================
%% Messages
%%======================================================================
message(User, {Timestamp, Type, From, To, Html}) ->
    {Date, Time}         = calendar:now_to_local_time(Timestamp),
    {Title, Link, Body}  = parse_legacy(Html),
    FromBin              = text(From),
    #{date    => Date,
      time    => Time,
      seconds => calendar:datetime_to_gregorian_seconds({Date, Time}),
      type    => Type,
      from    => FromBin,
      to      => recipients(To),
      sent    => Type =:= msgEntry andalso FromBin =:= User,
      user    => User,
      title   => Title,
      link    => Link,
      body    => Body}.

text(Value) when is_binary(Value) -> Value;
text(Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end;
text(_) -> <<>>.

recipients(To) when is_list(To) ->
    [Name || Name <- [text(Item) || Item <- To,
                                    is_list(Item) orelse is_binary(Item)],
             Name =/= <<>>];
recipients(_) -> [].

%% Identical reminders and system notices in a row become one card.
collapse([First = #{type := Type}, Second | Rest]) when Type =/= msgEntry ->
    case same_card(First, Second) of
        true ->
            Count = maps:get(count, First, 1) + 1,
            collapse([Second#{count => Count} | Rest]);
        false ->
            [First | collapse([Second | Rest])]
    end;
collapse([First | Rest]) -> [First | collapse(Rest)];
collapse([]) -> [].

same_card(#{type := Type, date := Date, title := Title, link := Link,
            body := Body},
          #{type := Type, date := Date, title := Title, link := Link,
            body := Body}) -> true;
same_card(_First, _Second) -> false.

timeline(Messages, Embedded) -> timeline(Messages, Embedded, undefined, []).

timeline([], _Embedded, _Previous, Acc) ->
    lists:reverse(Acc);
timeline([Message = #{date := Date} | Rest], Embedded, Previous, Acc) ->
    {Separator, Continued} =
        case Previous of
            #{date := Date} -> {[], continues(Previous, Message)};
            _               -> {[day_separator(Date)], false}
        end,
    Entry = entry(Message, Continued, Embedded),
    timeline(Rest, Embedded, Message, [Entry | Separator ++ Acc]).

continues(#{type := msgEntry, from := From, to := To, seconds := Before},
          #{type := msgEntry, from := From, to := To, seconds := After}) ->
    After - Before =< ?groupSeconds;
continues(_Previous, _Message) -> false.

day_separator(Date) ->
    {<<"div">>, [{<<"class">>, <<"day">>}],
     [{<<"span">>, [], [day_label(Date)]}]}.

day_label(Date) ->
    Today = calendar:date_to_gregorian_days(date()),
    case Today - calendar:date_to_gregorian_days(Date) of
        0 -> <<"Today">>;
        1 -> <<"Yesterday">>;
        _ ->
            {Year, Month, Day} = Date,
            WeekDay = element(calendar:day_of_the_week(Date),
                              {"Monday", "Tuesday", "Wednesday", "Thursday",
                               "Friday", "Saturday", "Sunday"}),
            MonthName = element(Month,
                                {"January", "February", "March", "April",
                                 "May", "June", "July", "August",
                                 "September", "October", "November",
                                 "December"}),
            iolist_to_binary(io_lib:format("~s ~B ~s ~B",
                                           [WeekDay, Day, MonthName, Year]))
    end.

entry(Message = #{type := msgEntry}, Continued, Embedded) ->
    chat_entry(Message, Continued, Embedded);
entry(Message, _Continued, Embedded) ->
    card_entry(Message, Embedded).

chat_entry(Message = #{sent := Sent, from := From, body := Body},
           Continued, Embedded) ->
    Side    = case Sent of
                  true  -> <<"sent">>;
                  false -> <<"received">>
              end,
    Classes = [<<"msg ">>, Side,
               case Continued of
                   true  -> <<" continued">>;
                   false -> <<>>
               end],
    Avatar  = case {Sent, Continued} of
                  {false, false} -> avatar_node(From);
                  {false, true}  -> {<<"span">>,
                                     [{<<"class">>, <<"avatar-space">>}], []};
                  {true, _}      -> []
              end,
    Meta    = case Continued of
                  true  -> [];
                  false -> [chat_meta(Message)]
              end,
    Bubble  = {<<"div">>, [{<<"class">>, bubble_class(Body)},
                           {<<"title">>, full_time(Message)}],
               link_targets(Body, Embedded) ++ [time_node(Message)]},
    {<<"article">>, [{<<"class">>, iolist_to_binary(Classes)}],
     lists:flatten([Avatar,
                    {<<"div">>, [{<<"class">>, <<"stack">>}],
                     Meta ++ [Bubble]}])}.

chat_meta(#{sent := true, to := To, user := User}) ->
    {<<"div">>, [{<<"class">>, <<"meta">>}],
     [{<<"span">>, [{<<"class">>, <<"to">>}],
       [<<"to ">>, names(To, User)]}]};
chat_meta(#{from := From, to := To, user := User}) ->
    Others = [Name || Name <- To, Name =/= User],
    Recipients = case Others of
                     [] -> [];
                     _ ->
                         [{<<"span">>, [{<<"class">>, <<"to">>}],
                           [<<"to ">>, names(To, User)]}]
                 end,
    {<<"div">>, [{<<"class">>, <<"meta">>}],
     [{<<"span">>, [{<<"class">>, <<"who">>}], [display_name(From)]}
      | Recipients]}.

names([], _User) -> <<"everyone">>;
names(Names, User) ->
    Shown = [case Name of
                 User -> <<"you">>;
                 _    -> Name
             end || Name <- Names],
    iolist_to_binary(lists:join(<<", ">>, Shown)).

display_name(<<>>) -> <<"Unknown">>;
display_name(Name) -> Name.

card_entry(Message = #{type := Type, title := Title, link := Link,
                       body := Body}, Embedded) ->
    {Class, Icon, Label} = card_style(Type, Title, Link),
    Count = case maps:get(count, Message, 1) of
                1 -> [];
                Times ->
                    TimesBin = integer_to_binary(Times),
                    [{<<"span">>,
                      [{<<"class">>, <<"count">>},
                       {<<"title">>,
                        <<"Repeated ", TimesBin/binary, " times">>}],
                      [<<"×"/utf8>>, TimesBin]}]
            end,
    Action = case Link of
                 {ok, Url} ->
                     [{<<"a">>, [{<<"class">>, <<"open-task">>},
                                 {<<"href">>, Url}],
                       [<<"Open task →"/utf8>>]}];
                 blocked -> []
             end,
    {<<"article">>, [{<<"class">>, <<"card-msg ", Class/binary>>}],
     [{<<"div">>, [{<<"class">>, <<"badge-icon">>},
                   {<<"aria-hidden">>, <<"true">>}], [Icon]},
      {<<"div">>, [{<<"class">>, <<"card-body">>},
                   {<<"title">>, full_time(Message)}],
       [{<<"div">>, [{<<"class">>, <<"meta">>}],
         [{<<"span">>, [{<<"class">>, <<"who">>}], [Label | Count]},
          time_node(Message)]},
        {<<"div">>, [{<<"class">>, <<"content">>}],
         link_targets(Body, Embedded)}
        | Action]}]}.

card_style(alarmEntry, <<"Timer expired">>, _Link) ->
    {<<"timer">>, <<"🍅"/utf8>>, <<"Timer expired">>};
card_style(alarmEntry, _Title, _Link) ->
    {<<"reminder">>, <<"⏰"/utf8>>, <<"Reminder">>};
card_style(_Type, _Title, {ok, _Url}) ->
    {<<"system task">>, <<"📌"/utf8>>, <<"Task update">>};
card_style(_Type, _Title, _Link) ->
    {<<"system">>, <<"🤖"/utf8>>, <<"eTodo">>}.

time_node(#{time := {Hour, Minute, _}}) ->
    {<<"time">>, [],
     [iolist_to_binary(io_lib:format("~2..0B:~2..0B", [Hour, Minute]))]}.

full_time(#{date := {Y, Mo, D}, time := {H, Mi, S}}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                   [Y, Mo, D, H, Mi, S])).

avatar_node(Name) ->
    case portrait(Name) of
        {ok, _Path} ->
            Src = iolist_to_binary(
                    ["/avatar?",
                     uri_string:compose_query([{<<"user">>, Name}])]),
            {<<"img">>, [{<<"class">>, <<"avatar">>}, {<<"src">>, Src},
                         {<<"alt">>, <<>>}], []};
        error ->
            Hue = integer_to_binary(erlang:phash2(Name, 360)),
            {<<"span">>, [{<<"class">>, <<"avatar initials">>},
                          {<<"style">>, <<"--hue:", Hue/binary>>},
                          {<<"aria-hidden">>, <<"true">>}],
             [initials(Name)]}
    end.

initials(Name) ->
    Words = string:lexemes(Name, " -_."),
    Letters = [string:slice(Word, 0, 1) || Word <- lists:sublist(Words, 2)],
    case iolist_to_binary(Letters) of
        <<>> -> <<"?">>;
        Initials -> string:uppercase(Initials)
    end.

bubble_class(Body) ->
    case jumbo(Body) of
        true  -> <<"bubble jumbo">>;
        false -> <<"bubble">>
    end.

%% A message that only contains one to three emoji gets big emoji.
jumbo(Body) ->
    Leaves = leaves(Body),
    Emoji = [x || emoji <- Leaves],
    Other = [x || {text, Text} <- Leaves, string:trim(Text) =/= <<>>],
    Emoji =/= [] andalso length(Emoji) =< 3 andalso Other =:= [].

leaves(Nodes) when is_list(Nodes) -> lists:flatmap(fun leaves/1, Nodes);
leaves({<<"span">>, [{<<"class">>, <<"emoji">>} | _], _}) -> [emoji];
leaves({<<"p">>, _, Children}) -> leaves(Children);
leaves({_Tag, _Attributes, _Children}) -> [{text, <<"x">>}];
leaves(Text) when is_binary(Text) ->
    [{text, binary:replace(Text, <<"\xC2\xA0">>, <<>>, [global])}].

%% Browser pages open external links in a new tab. The desktop WebView
%% vetoes the navigation and launches the default browser instead.
link_targets(Nodes, true) -> Nodes;
link_targets(Nodes, false) when is_list(Nodes) ->
    [link_targets(Node, false) || Node <- Nodes];
link_targets({<<"a">>, Attributes = [{<<"href">>, _}, {<<"rel">>, _} | _],
              Children}, false) ->
    {<<"a">>, Attributes ++ [{<<"target">>, <<"_blank">>}], Children};
link_targets({Tag, Attributes, Children}, false) ->
    {Tag, Attributes, link_targets(Children, false)};
link_targets(Text, false) -> Text.

%%======================================================================
%% Legacy HTML parsing
%%======================================================================
parse_legacy(Html) ->
    try mochiweb_html:parse(<<"<div>", Html/binary, "</div>">>) of
        Tree ->
            Fonts   = fonts([Tree]),
            Headers = [Children || {header, Children} <- Fonts],
            Bodies  = [Children || {body, Children} <- Fonts],
            Body    = case Bodies of
                          [Content | _] -> sanitize(Content);
                          []            -> [plain([Tree])]
                      end,
            {title(Headers), first_link(Headers), Body}
    catch
        _:_ -> {<<>>, blocked, [<<"(This message could not be shown)">>]}
    end.

fonts(Nodes) -> lists:flatmap(fun font/1, Nodes).

font({<<"font">>, Attributes, Children}) ->
    Face = string:lowercase(attribute(<<"face">>, Attributes)),
    case Face of
        <<"impact", _/binary>> -> [{header, Children}];
        <<"tahoma", _/binary>> -> [{body, Children}];
        _                      -> fonts(Children)
    end;
font({_Tag, _Attributes, Children}) when is_list(Children) ->
    fonts(Children);
font(_) -> [].

title(Headers) ->
    Texts = [string:trim(plain(Header)) || Header <- Headers],
    case [Text || Text <- Texts, not is_time(Text)] of
        [Title | _] -> Title;
        []          -> <<>>
    end.

is_time(Text) ->
    re:run(Text, "^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$", [{capture, none}])
        =:= match.

first_link(Nodes) when is_list(Nodes) ->
    lists:foldl(fun(Node, blocked) -> first_link(Node);
                   (_Node, Found) -> Found
                end, blocked, Nodes);
first_link({<<"a">>, Attributes, _Children}) ->
    eWorkLogView:legacy_link(attribute(<<"href">>, Attributes));
first_link({_Tag, _Attributes, Children}) when is_list(Children) ->
    first_link(Children);
first_link(_) -> blocked.

plain(Nodes) when is_list(Nodes) ->
    iolist_to_binary([plain(Node) || Node <- Nodes]);
plain({_Tag, _Attributes, Children}) when is_list(Children) ->
    plain(Children);
plain(Text) when is_binary(Text) -> Text;
plain(_) -> <<>>.

attribute(Name, Attributes) ->
    case lists:keyfind(Name, 1, Attributes) of
        {Name, Value} when is_binary(Value) -> Value;
        _ -> <<>>
    end.

%%======================================================================
%% Sanitizer: only an allowlist of tags survives, attributes are rebuilt.
%%======================================================================
sanitize(Nodes) when is_list(Nodes) -> lists:flatmap(fun clean/1, Nodes).

clean(Text) when is_binary(Text) -> [Text];
clean({<<"a">>, Attributes, Children}) ->
    link_node(attribute(<<"href">>, Attributes), sanitize(Children));
clean({<<"img">>, Attributes, _Children}) ->
    image(attribute(<<"src">>, Attributes),
          attribute(<<"alt">>, Attributes));
clean({Tag, Attributes, Children}) when is_binary(Tag),
                                        is_list(Children) ->
    case {lists:member(Tag, ?keepTags), lists:member(Tag, ?dropTags)} of
        {true, _}      -> [{Tag, cell_attributes(Tag, Attributes),
                            sanitize(Children)}];
        {false, true}  -> [];
        {false, false} -> sanitize(Children)
    end;
clean(_) -> [].

cell_attributes(Tag, Attributes) when Tag =:= <<"td">>; Tag =:= <<"th">> ->
    [{Name, Value} || {Name, Value} <- Attributes,
                      Name =:= <<"colspan">> orelse Name =:= <<"rowspan">>,
                      re:run(Value, "^[1-9][0-9]?$", [{capture, none}])
                          =:= match];
cell_attributes(_Tag, _Attributes) -> [].

link_node(Href, Children) ->
    case eWorkLogView:legacy_link(Href) of
        {ok, Url} ->
            [{<<"a">>, [{<<"href">>, Url}, {<<"class">>, <<"task-link">>}],
              Children}];
        blocked ->
            case external(Href) of
                true ->
                    [{<<"a">>, [{<<"href">>, Href},
                                {<<"rel">>, <<"noopener noreferrer">>}],
                      Children}];
                false -> Children
            end
    end.

image(Src, Alt) ->
    case emote(Src) of
        {ok, Emoji} ->
            Label = case Alt of
                        <<>> -> Emoji;
                        _    -> Alt
                    end,
            [{<<"span">>, [{<<"class">>, <<"emoji">>}, {<<"role">>, <<"img">>},
                           {<<"aria-label">>, Label}, {<<"title">>, Label}],
              [Emoji]}];
        error ->
            Label = case Alt of
                        <<>> -> <<"Image">>;
                        _    -> Alt
                    end,
            Content = [<<"🖼️ "/utf8>>, Label],
            case external(Src) of
                true ->
                    [{<<"a">>, [{<<"href">>, Src},
                                {<<"rel">>, <<"noopener noreferrer">>},
                                {<<"class">>, <<"image-link">>}],
                      Content}];
                false ->
                    [{<<"span">>, [{<<"class">>, <<"image-link">>}], Content}]
            end
    end.

emote(Src) ->
    case binary:match(Src, <<"/Icons/">>) of
        nomatch -> error;
        _       -> maps:find(filename:basename(Src), ?emotes)
    end.

external(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme, host := Host} when Host =/= <<>> ->
            lists:member(string:lowercase(Scheme), [<<"http">>, <<"https">>])
                andalso re:run(Url, "[\\x00-\\x20\"'<>\\\\]",
                               [{capture, none}]) =:= nomatch;
        #{scheme := Scheme, path := Path} when Path =/= <<>> ->
            string:lowercase(Scheme) =:= <<"mailto">> andalso
                re:run(Url, "[\\x00-\\x20\"'<>\\\\]",
                       [{capture, none}]) =:= nomatch;
        _ -> false
    catch
        _:_ -> false
    end.
