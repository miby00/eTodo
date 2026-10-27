%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 18 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eHtml).

%% API
-export([generateMsg/4,
         generateSystemMsg/2,
         generateSystemMsgEmail/3,
         generateAlarmMsg/2,
         generateAlarmMsgEmail/3,
         printTaskList/5,
         makeWorkLogReport/2,
         makeTimeLogReport/3,
         makeScheduleReport/1]).

-export([htmlTag/0, htmlTag/1, htmlTag/2,
         headTag/0, headTag/1, headTag/2,
         bodyTag/0, bodyTag/1, bodyTag/2,
         titleTag/1, titleTag/2,
         styleTag/1, styleTag/2,
         tableTag/0, tableTag/1, tableTag/2,
         divTag/0, divTag/1, divTag/2,
         spanTag/0, spanTag/1, spanTag/2,
         fontTag/0, fontTag/1, fontTag/2,
         pTag/0, pTag/1, pTag/2,
         bTag/0, bTag/1, bTag/2,
         tdTag/0, tdTag/1, tdTag/2,
         thTag/0, thTag/1, thTag/2,
         trTag/0, trTag/1, trTag/2,
         brTag/0,
         formTag/0, formTag/1, formTag/2,
         aTag/0, aTag/1, aTag/2,
         selectTag/1, selectTag/2, inputTag/1,
         metaTag/1, imgTag/1]).

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("inets/include/httpd.hrl").

-define(Background,  "#E0E0E0").
-export([themeMessages/5]).

-define(TextColor,   "#303030").
-define(HdrOptions1, [{color, ?TextColor}, {size, 3},
                      {face, "Impact, Charcoal, sans-serif"}]).
-define(HdrOptions2,  [{color, ?TextColor}, {size, 3},
                       {face, "Impact, Charcoal, sans-serif"}]).
-define(MsgOptions1,  [{color, "Black"}, {size, 3},
                       {face, "Tahoma, Geneva, sans-serif"}]).

-define(MsgOptions2,  [{color, "Red"}, {size, 3},
                       {face, "Tahoma, Geneva, sans-serif"}]).

-define(ValidURLChars, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        "0123456789-._~:/?#[]@!$&'*+,;=`.").

-import(eTodoUtils, [toStr/1, toStr/2, tryInt/1, getWeekDay/1,
                     makeStr/1, getRootDir/0, dateTime/0,
                     convertUid/1, convertUid/2, default/2,
                     getStyleSheet/1]).

%%%=====================================================================
%%% API
%%%=====================================================================

themeMessages(Html, Background, Foreground, Bubble, Link) ->
    Content = unicode:characters_to_binary(Html),
    Tree = mochiweb_html:parse(
             <<"<html><body>", Content/binary, "</body></html>">>),
    Theme = #{background => Background, foreground => Foreground,
              bubble => Bubble, link => Link},
    mochiweb_html:to_html(themeMessageNode(Tree, Theme)).

themeMessageNode({<<"body">>, _Attributes, Children}, Theme) ->
    Attributes = [{<<"bgcolor">>, maps:get(background, Theme)},
                  {<<"text">>, maps:get(foreground, Theme)},
                  {<<"link">>, maps:get(link, Theme)}],
    {<<"body">>, Attributes,
     [themeMessageNode(Child, Theme) || Child <- Children]};
themeMessageNode({Tag, Attributes, Children}, Theme) ->
    {Tag, [themeMessageAttribute(Attribute, Theme)
           || Attribute <- Attributes],
     [themeMessageNode(Child, Theme) || Child <- Children]};
themeMessageNode(Node, _Theme) -> Node.

themeMessageAttribute({Name, Value} = Attribute, Theme) ->
    case {Name, string:lowercase(Value)} of
        {<<"bgcolor">>, <<"#e0e0e0">>} ->
            {Name, maps:get(bubble, Theme)};
        {<<"color">>, Color} when Color =:= <<"#303030">>;
                                  Color =:= <<"black">> ->
            {Name, maps:get(foreground, Theme)};
        _ -> Attribute
    end.

%%======================================================================
%% Make functions of HTML tags so we can produce html by calling them.
%%======================================================================

htmlTag() -> tag(html, [], []).
htmlTag(Content) -> tag(html, [], Content).
htmlTag(Attr, Content) -> tag(html, Attr, Content).

headTag() -> tag(head, [], []).
headTag(Content) -> tag(head, [], Content).
headTag(Attr, Content) -> tag(head, Attr, Content).

bodyTag() -> tag(body, [], []).
bodyTag(Content) -> tag(body, [], Content).
bodyTag(Attr, Content) -> tag(body, Attr, Content).

titleTag(Content) -> tag(title, [], Content).
titleTag(Attr, Content) -> tag(title, Attr, Content).

styleTag(Content) -> tag(style, [], Content).
styleTag(Attr, Content) -> tag(style, Attr, Content).

tableTag() -> tag(table, [], []).
tableTag(Content) -> tag(table, [], Content).
tableTag(Attr, Content) -> tag(table, Attr, Content).

divTag() -> tag('div', [], []).
divTag(Content) -> tag('div', [], Content).
divTag(Attr, Content) -> tag('div', Attr, Content).

spanTag() -> tag(span, [], []).
spanTag(Content) -> tag(span, [], Content).
spanTag(Attr, Content) -> tag(span, Attr, Content).

fontTag() -> tag(font, [], []).
fontTag(Content) -> tag(font, [], Content).
fontTag(Attr, Content) -> tag(font, Attr, Content).

aTag() -> tag(a, [], []).
aTag(Content) -> tag(a, [], Content).
aTag(Attr, Content) -> tag(a, Attr, Content).

pTag() -> tag(p, [], []).
pTag(Content) -> tag(p, [], Content).
pTag(Attr, Content) -> tag(p, Attr, Content).

bTag() -> tag(b, [], []).
bTag(Content) -> tag(b, [], Content).
bTag(Attr, Content) -> tag(b, Attr, Content).

trTag() -> tag(tr, [], []).
trTag(Content) -> tag(tr, [], Content).
trTag(Attr, Content) -> tag(tr, Attr, Content).

tdTag() -> tag(td, [], []).
tdTag(Content) -> tag(td, [], Content).
tdTag(Attr, Content) -> tag(td, Attr, Content).

thTag() -> tag(th, [], []).
thTag(Content) -> tag(th, [], Content).
thTag(Attr, Content) -> tag(th, Attr, Content).

formTag() -> tag(form, [], []).
formTag(Content) -> tag(form, [], Content).
formTag(Attr, Content) -> tag(form, Attr, Content).

selectTag(Content) -> tag(select, [], Content).
selectTag(Attr, Content) -> tag(select, Attr, Content).

inputTag(Attr) -> cTag(input, Attr).

imgTag(Attr) -> cTag(img, Attr).

metaTag(Attr) -> cTag(meta, Attr).

brTag() -> cTag(br, []).

tag(Tag, Attr, Content) ->
    HTag = [$<, toStr(Tag), attr(Attr), $>, Content, $<, $/, toStr(Tag), $>],
    case lists:keyfind(nonEmpty, 1, Attr) of
        {nonEmpty, Value} ->
            nonEmpty(Value, HTag);
        false ->
            HTag
    end.

cTag(Tag, Attr) ->
    [$<, toStr(Tag), attr(Attr), 32, $/, $>].

attr([]) ->
    [];
attr(Attr) ->
    attr(Attr, []).

attr([], Acc) ->
    Acc;
attr([{nonEmpty, _} | Rest], Acc) ->
    attr(Rest, Acc);
attr([{Name, Value} | Rest], Acc) when is_integer(Value) ->
    attr(Rest, [Acc, 32, toStr(Name), $=, toStr(Value)]);
attr([{Name, Value} | Rest], Acc) when is_list(Value) ->
    case lists:member($', Value) of
        false ->
            attr(Rest, [Acc, 32, toStr(Name), $=, $', toStr(Value), $']);
        true ->
            attr(Rest, [Acc, 32, toStr(Name), $=, $", toStr(Value), $"])
    end;
attr([{Name, Value} | Rest], Acc) ->
    attr(Rest, [Acc, 32, toStr(Name), $=, $', toStr(Value), $']);
attr([Field | Rest], Acc) ->
    attr(Rest, [Acc, 32, Field]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
printTaskList(User, List, Filter, SearchText, Cfg) ->
    ETodos = eTodoDB:getETodos(User, List, Filter, SearchText, Cfg),
    Body = [[makePrintTaskList(ETodo), pTag()] || ETodo <- ETodos],
    ["<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN' ",
     "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>",
     htmlTag([{"xmlns", "http://www.w3.org/1999/xhtml"},
              {"xml:lang", "en"},
              {"lang", "en"}],
             [headTag([metaTag([{"http-equiv", "Content-Type"},
                                {content, "text/html; charset=UTF-8"}]),
                       titleTag("eTodo - " ++ User)]),
              bodyTag(Body)])].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makePrintTaskList(#etodo{status = Status,
                         priority = Priority,
                         dueTime = DueTime,
                         description = Description,
                         comment = Comment,
                         sharedWith = SharedWith,
                         createTime = CreateTime,
                         doneTime = DoneTime,
                         progress = Progress,
                         owner = Owner,
                         uid = Uid}) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    AllLoggedWork = eTodoDB:getAllLoggedWork(Uid),

    [tableTag([{width, "100%"}, {bgcolor, "#b9c9fe"}],
              [trTag([headerCell(?status),
                      dataCell(Status, [{width, "26%"}]),
                      headerCell(?prio),
                      dataCell(Priority, [{width, "18%"}]),
                      headerCell("Progress(%)"),
                      dataCell(toStr(Progress), [{width, "10%"}])]),
               trTag([headerCell(?createTime),
                      dataCell(CreateTime, [{width, "26%"}]),
                      headerCell(?dueTime),
                      dataCell(DueTime, [{width, "18%"}]),
                      headerCell("Estimate(h)"),
                      dataCell(toStr(Estimate), [{width, "10%"}])]),
               trTag([
                      headerCell(?doneTimestamp),
                      dataCell(DoneTime, [{width, "26%"}]),
                      headerCell("Logged(h)"),
                      dataCell(AllLoggedWork, [{width, "18%"}]),
                      headerCell("Remaining(h)"),
                      dataCell(toStr(Remaining), [{width, "10%"}])]),
               trTag([headerCell(?owner),
                      dataCell(Owner, [{width, "26%"}]),
                      headerCell(?sharedWith),
                      dataCell(SharedWith, [{colspan, 3}])]),
               trTag([{nonEmpty, Description}],
                     [headerCell(?description),
                      dataCell2(Description, [{colspan, 5}])]),
               trTag([{nonEmpty, Comment}],
                     [headerCell(?comment),
                      dataCell2(Comment, [{colspan, 5}])])])].

headerCell(Text) ->
    tdTag([{width, "15%"}, {bgcolor, "#b9c9fe"}],
          fontTag([{size, 1}], bTag([Text, $:]))).

dataCell(Text, Extra) ->
    tdTag([{bgcolor, "#e8edff"} | Extra], fontTag([{size, 1}], makeHtml(Text))).

dataCell2(Text, Extra) ->
    tdTag([{bgcolor, "#e8edff"} | Extra], fontTag([{size, 1}],
                                                  eMd2Html:convert(Text))).

%%======================================================================
%% Function : makeWorkLogReport(User, Date) -> Html
%% Purpose  : Make html report of work log.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeWorkLogReport(User, Date) ->
    {D1, D2, D3, D4, D5, Act4} = calcReport(User, Date),
    Opts = [{width, "14%"}, {align, center}],
    tableTag([
              makeWorkLogReport(Date, Act4, {D1, D2, D3, D4, D5}, []),
              trTag([{bgcolor, "black"}],
                    [tdTag([{width, "30%"}], heading("Total")),
                     tdTag(Opts, heading(tot(D1))),
                     tdTag(Opts, heading(tot(D2))),
                     tdTag(Opts, heading(tot(D3))),
                     tdTag(Opts, heading(tot(D4))),
                     tdTag(Opts, heading(tot(D5)))
                    ])]).


calcReport(User, Date) ->
    D1 = eTodoDB:getLoggedWork(User, Date),
    D2 = eTodoDB:getLoggedWork(User, incDate(Date, 1)),
    D3 = eTodoDB:getLoggedWork(User, incDate(Date, 2)),
    D4 = eTodoDB:getLoggedWork(User, incDate(Date, 3)),
    D5 = eTodoDB:getLoggedWork(User, incDate(Date, 4)),
    All = lists:flatten([D1, D2, D3, D4, D5]),
    Act1 = [Task || {Task, _H, _M} <- All],
    Act2 = lists:usort(Act1),
    Act3 = [{Task, eTodoDB:getWorkDesc(Task)} || Task <- Act2],
    Act4 = lists:keysort(2, Act3),
    {D1, D2, D3, D4, D5, Act4}.

tot(Days) ->
    tot(Days, {0, 0}).

tot([], {SumHours, SumMin}) ->
    SumHours2 = SumHours + (SumMin div 60),
    SumMinutes2 = SumMin rem 60,
    time(SumHours2) ++ ":" ++ time(SumMinutes2);
tot([{_, Hours, Min} | Rest], {SumHours, SumMin}) ->
    tot(Rest, {SumHours + Hours, SumMin + Min}).

incDate(Date, Inc) ->
    Days = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(Days + Inc).




makeWorkLogReport(Date, [], _Days, Result) ->
    Opts = [{width, "14%"}, {align, center}],
    [trTag([{bgcolor, "black"}],
           [tdTag([{width, "30%"}], heading("Task description")),
            tdTag(Opts, heading(getWeekDay(Date))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 1)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 2)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 3)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 4))))
           ]) | lists:reverse(Result)];
makeWorkLogReport(Date, [{Act, Desc} | Rest],
                  Days = {D1, D2, D3, D4, D5}, SoFar) ->
    Odd = ((length(Rest) rem 2) == 0),
    Opts = [{align, center}],
    Uid = tryInt(Act),
    UidStr = convertUid(Uid),

    Link1 = makeLink(Uid, Date, Act, D1),
    Link2 = makeLink(Uid, incDate(Date, 1), Act, D2),
    Link3 = makeLink(Uid, incDate(Date, 2), Act, D3),
    Link4 = makeLink(Uid, incDate(Date, 3), Act, D4),
    Link5 = makeLink(Uid, incDate(Date, 4), Act, D5),

    Row = trTag(bgColor(Odd),
                [tdTag(aTag([{href, UidStr}], empty(Desc, Act))),
                 tdTag(Opts, Link1),
                 tdTag(Opts, Link2),
                 tdTag(Opts, Link3),
                 tdTag(Opts, Link4),
                 tdTag(Opts, Link5)]),
    makeWorkLogReport(Date, Rest, Days, [Row | SoFar]).

makeLink(Uid, Date, Act, Day) ->
    Time1 = hours(Act, Day) ++ ":" ++ minutes(Act, Day),
    Time2 = spanTag([{style, "color:grey;"}], Time1),
    case Time1 of
        "00:00" ->
            aTag([{href, convertUid(Uid, Date)}], Time2);
        _ ->
            aTag([{href, convertUid(Uid, Date)}], Time1)
    end.

empty("", Value) -> Value;
empty(undefined, Value) -> Value;
empty(Value, _) -> makeHtml(Value).

bgColor(false) ->
    [{bgcolor, "white"}];
bgColor(true) ->
    [{bgcolor, "#C0C0C0"}].

heading(Content) ->
    fontTag([{color, "white"}], Content).

hours(Act, Day) ->
    case lists:keyfind(Act, 1, Day) of
        false ->
            "00";
        {_, Hours, _} ->
            time(Hours)
    end.

minutes(Act, Day) ->
    case lists:keyfind(Act, 1, Day) of
        false ->
            "00";
        {_, _, Minutes} ->
            time(Minutes)
    end.

time(Min) when Min < 10 ->
    "0" ++ integer_to_list(Min);
time(Min) ->
    integer_to_list(Min).
















%% Sorting alternatives
%% ?descDef, ?status, ?prio, ?description, ?createTime





nonEmpty([], _) ->
    [];
nonEmpty(undefined, _) ->
    [];
nonEmpty(_, Value) ->
    Value.













%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
generateMsg(Sender, Sender, Users, Text) ->
    Icon      = getIcon(Users),
    Portrait  = getPortrait(Sender),
    WPortrait = getWebPortrait(Sender),
    UserList  = case Users of
                    [] ->
                        "";
                    _ ->
                        [" to ", makeHtml(makeStr(Users))]
                end,
    {tableTag([{align, "right"}, {width, "100%"},
               {cellspacing, "0"}, {cellpadding, "10"}],
              [trTag([tdTag([{colspan, 4}], [])]),
               trTag(
                 [tdTag([{width, "150"}], []),
                  tdTag([{valign, "top"}, {align, "left"},
                         {width, "100%"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1, [Sender, UserList])),
                  tdTag([{valign, "top"}, {width, "80"},
                         {align, "center"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1, toStr(time(), time))),
                  tdTag([{valign, "top"}, {width, "40"}],
                        imgTag([{src, getRootDir() ++ "/Icons/" ++ Icon}]))]),
               trTag(
                 [tdTag(),
                  tdTag([{align, "left"}, {bgcolor, ?Background}],
                        fontTag(?MsgOptions1, eMd2Html:convert(Text))),
                  tdTag([{valign, "top"}, {align, center}, {bgcolor, ?Background}],
                        imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                  tdTag()])]),
     tableTag([{class, "msgSent"}],
              [trTag(
                 [tdTag([{class, "msgText"}], [Sender, UserList]),
                  tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
                  tdTag([{class, "msgImg"}],
                        imgTag([{src, "/priv/Icons/" ++ Icon}]))
                 ]),
               trTag(
                 [tdTag(eMd2Html:convert(Text, "/priv")),
                  tdTag([{class, "portraitCol"}],
                        imgTag([{src, WPortrait}, {class, "portrait"}])),
                  tdTag()])])};
generateMsg(_User, Sender, Users, Text) ->
    Icon = getIcon(Users),
    Portrait = getPortrait(Sender),
    WPortrait = getWebPortrait(Sender),
    UserList = case Users of
                   [] ->
                       "";
                   _ ->
                       [" to ", makeHtml(makeStr(Users))]
               end,
    {tableTag([{align, "left"}, {width, "100%"},
               {cellspacing, "0"}, {cellpadding, "10"}],
              [trTag([tdTag([{colspan, 4}, {height, "5"}], [])]),
               trTag(
                 [tdTag([{valign, "top"}, {width, "40"}],
                        imgTag([{src, getRootDir() ++ "/Icons/" ++ Icon}])),
                  tdTag([{valign, "top"}, {width, "80"},
                         {align, "center"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1, toStr(time(), time))),
                  tdTag([{valign, "top"}, {width, "100%"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1, [Sender, UserList])),
                  tdTag([{width, "150"}], [])]),
               trTag(
                 [tdTag(),
                  tdTag([{valign, "top"}, {align, center}, {bgcolor, ?Background}],
                        imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                  tdTag([{bgcolor, ?Background}],
                        fontTag(?MsgOptions1, eMd2Html:convert(Text))),
                  tdTag()])]),
     tableTag([{class, "msgReceived"}],
              [trTag(
                 [tdTag([{class, "msgImg"}],
                        imgTag([{src, "/priv/Icons/" ++ Icon}])),
                  tdTag([{class, "msgTime textCenter"}],
                        toStr(time(), time)),
                  tdTag([{class, "msgText"}], [Sender, UserList])]),
               trTag(
                 [tdTag(),
                  tdTag([{class, "portraitCol"}],
                        imgTag([{src, WPortrait}, {class, "portrait"}])),
                  tdTag(eMd2Html:convert(Text, "/priv"))])])}.

getIcon(Users) when length(Users) =< 1 -> "userChat.png";
getIcon(_Users) -> "multiple.png".

getWebPortrait(User) ->
    CustomPortrait = getRootDir() ++ "/www/priv/Icons/portrait_" ++ User ++ ".png",
    case filelib:is_file(CustomPortrait) of
        true ->
            "/priv/Icons/portrait_" ++ User ++ ".png";
        false ->
            "/priv/Icons/portrait.png"
    end.

getPortrait(User) ->
    CustomPortrait = getRootDir() ++ "/Icons/portrait_" ++ User ++ ".png",
    case filelib:is_file(CustomPortrait) of
        true ->
            getRootDir() ++ "/Icons/portrait_" ++ User ++ ".png";
        false ->
            getRootDir() ++ "/Icons/portrait.png"
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
generateSystemMsg(system, Text) ->
    Portrait  = getRootDir() ++ "/Icons/eTodo.png",
    WPortrait = "/priv/Icons/eTodoBiggest.png",
    {tableTag([{cellspacing, "0"}, {cellpadding, "10"}],
              [trTag([tdTag([{colspan, 3}], [])]),
               trTag(
                 [tdTag([{valign, "top"}],
                        imgTag([{src, getRootDir() ++ "/Icons/etodoChat.png"}])),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1,
                                toStr(time(), time))),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1, "eTodo"))]),
               trTag(
                   [tdTag(),
                    tdTag([{valign, "top"}, {align, center}, {bgcolor, ?Background}],
                          imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                    tdTag([{bgcolor, ?Background}],
                          fontTag(?MsgOptions1, eMd2Html:convert(Text)))])]),
     tableTag(
       [trTag(
          [tdTag([{class, "msgImg"}],
                 imgTag([{src, "/priv/Icons/etodoChat.png"}])),
           tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
           tdTag([{class, "msgText"}], "eTodo")]),
        trTag(
            [tdTag(),
             tdTag([{class, "portraitCol"}],
                   imgTag([{src, WPortrait}, {class, "portrait"}])),
             tdTag(eMd2Html:convert(Text, "/priv"))])])};
generateSystemMsg(Uid, Text) ->
    UidStr = eTodoUtils:convertUid(Uid),
    Portrait = getRootDir() ++ "/Icons/eTodo.png",
    WPortrait = "/priv/Icons/eTodoBiggest.png",
    {tableTag([{cellspacing, "0"}, {cellpadding, "10"}],
              [trTag([tdTag([{colspan, 3}], [])]),
               trTag(
                 [tdTag([{valign, "top"}],
                        imgTag([{src, getRootDir() ++ "/Icons/etodoChat.png"}])),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1,
                                toStr(time(), time))),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions1,
                                aTag([{href, UidStr}], "eTodo")))]),
               trTag(
                   [tdTag(),
                    tdTag([{valign, "top"}, {align, center}, {bgcolor, ?Background}],
                          imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                    tdTag([{bgcolor, ?Background}],
                          fontTag(?MsgOptions1, eMd2Html:convert(Text)))])]),
     tableTag(
       [trTag(
          [tdTag([{class, "msgImg"}],
                 imgTag([{src, "/priv/Icons/etodoChat.png"}])),
           tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
           tdTag([{class, "msgText"}],
                 aTag([{href, "/eTodo/eWeb:showTodo?uid=" ++
                            uri_string:quote(UidStr)}], "eTodo"))]),
        trTag(
            [tdTag(),
             tdTag([{class, "portraitCol"}],
                   imgTag([{src, WPortrait}, {class, "portrait"}])),
             tdTag(eMd2Html:convert(Text, "/priv"))])])}.

generateSystemMsgEmail(User, Uid, Text) ->
    ExtUrl    = constructExternalUrl(User, Uid),
    WPortrait = "/priv/Icons/eTodoBiggest.png",

    tableTag(
      [trTag(
         [tdTag([{class, "msgImg"}],
                imgTag([{src, "/priv/Icons/etodoChat.png"}])),
          tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
          tdTag([{class, "msgText"}], aTag([{href, ExtUrl}], "eTodo"))]),
       trTag(
           [tdTag(),
            tdTag([{class, "portraitCol"}],
                  imgTag([{src, WPortrait}, {class, "portrait"}])),
            tdTag(eMd2Html:convert(Text, "/priv"))])]).

constructExternalUrl(User, Uid) ->
    UidStr   = eTodoUtils:convertUid(Uid),
    Port     = application:get_env(eTodo, mistWebPort, 9003),
    ConCfg   = default(eTodoDB:getConnection(User),
                       #conCfg{host = "localhost"}),
    Host     = default(ConCfg#conCfg.host, "localhost"),

    "https://" ++ Host ++ ":" ++ integer_to_list(Port) ++
        "/#task-" ++ uri_string:quote(UidStr).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
generateAlarmMsg(timer, Text) ->
    Portrait  = getRootDir() ++ "/Icons/eTodo.png",
    WPortrait = "/priv/Icons/eTodoBiggest.png",

    {tableTag([{cellspacing, "0"}, {cellpadding, "10"}],
              [trTag([tdTag([{colspan, 3}], [])]),
               trTag(
                 [tdTag([{valign, "top"}],
                        imgTag([{src, getRootDir() ++ "/Icons/clockChat.png"}])),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions2,
                                toStr(time(), time))),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions2, "Timer expired"))]),
               trTag(
                   [tdTag(),
                    tdTag([{valign, "top"}, {align, center}, {bgcolor, ?Background}],
                          imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                    tdTag([{bgcolor, ?Background}],
                          fontTag(?MsgOptions1, eMd2Html:convert(Text)))])]),
     tableTag(
       [trTag(
          [tdTag([{class, "msgImg"}],
                 imgTag([{src, "/priv/Icons/clockChat.png"}])),
           tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
           tdTag([{class, "msgText"}], "Timer expired")]),
        trTag(
            [tdTag(),
             tdTag([{class, "portraitCol"}],
                   imgTag([{src, WPortrait}, {class, "portrait"}])),
             tdTag(eMd2Html:convert(Text, "/priv"))])])};
generateAlarmMsg(Uid, Text) ->
    Portrait  = getRootDir() ++ "/Icons/eTodo.png",
    WPortrait = "/priv/Icons/eTodoBiggest.png",
    UidStr = eTodoUtils:convertUid(Uid),
    {tableTag([{cellspacing, "0"}, {cellpadding, "10"}],
              [trTag([tdTag([{colspan, 3}], [])]),
               trTag(
                 [tdTag([{valign, "top"}],
                        imgTag([{src, getRootDir() ++ "/Icons/clockChat.png"}])),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions2,
                                toStr(time(), time))),
                  tdTag([{valign, "top"}, {bgcolor, ?Background}],
                        fontTag(?HdrOptions2,
                                aTag([{href, UidStr}], "eTodo")))]),
               trTag(
                   [tdTag(),
                    tdTag([{valign, "top"}, {align, center}, {bgcolor, ?Background}],
                          imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                    tdTag([{bgcolor, ?Background}],
                          fontTag(?MsgOptions1, eMd2Html:convert(Text)))])]),
     tableTag([{class, "msgAlarm"}],
              [trTag(
                 [tdTag([{class, "msgImg"}],
                        imgTag([{src, "/priv/Icons/clockChat.png"}])),
                  tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
                  tdTag([{class, "msgText"}],
                        aTag([{href, "/eTodo/eWeb:showTodo?uid=" ++
                                   uri_string:quote(UidStr)}], "eTodo"))]),
               trTag(
                   [tdTag(),
                    tdTag([{class, "portraitCol"}],
                          imgTag([{src, WPortrait}, {class, "portrait"}])),
                    tdTag(eMd2Html:convert(Text, "/priv"))])])}.

generateAlarmMsgEmail(User, Uid, Text) ->
    ExtUrl    = constructExternalUrl(User, Uid),
    WPortrait = "/priv/Icons/eTodoBiggest.png",
    tableTag([{class, "msgAlarm"}],
             [trTag(
                [tdTag([{class, "msgImg"}],
                       imgTag([{src, "/priv/Icons/clockChat.png"}])),
                 tdTag([{class, "msgTime textCenter"}], toStr(time(), time)),
                 tdTag([{class, "msgText"}],
                       aTag([{href, ExtUrl}], "eTodo"))]),
              trTag(
                  [tdTag(),
                   tdTag([{class, "portraitCol"}],
                         imgTag([{src, WPortrait}, {class, "portrait"}])),
                   tdTag(eMd2Html:convert(Text, "/priv"))])]).


%%%===================================================================
%%% Callbacks
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

makeHtml(Text) ->
    makeHtml(Text, getRootDir()).

makeHtml(Text, Dir) ->
    eGuiFunctions:filterAndConvert2Utf8(checkForLinks(Text, Dir)).

checkForLinks(Text, Dir) ->
    checkForLinks(Text, Dir, [], text, []).

checkForLinks("", Dir, _Token, text, Acc) ->
    html(lists:flatten(Acc), Dir, []);
checkForLinks("", Dir, Token, {link, 0}, Acc) ->
    case checkToken(Token, 32) of
        {true, Link} ->
            html(lists:flatten(Acc), Dir, []) ++ Link;
        false ->
            html(lists:flatten([Acc, Token]), Dir, [])
    end;
checkForLinks("", Dir, Token, {link, _Num}, Acc) ->
    html(lists:flatten([Acc, Token]), Dir, []);

checkForLinks("https://" ++ Rest, Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "https://", {link, 0}, Acc);
checkForLinks("HTTPS://" ++ Rest, Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "HTTPS://", {link, 0}, Acc);
checkForLinks("Https://" ++ Rest, Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "Https://", {link, 0}, Acc);
checkForLinks("http://" ++ Rest, Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "http://", {link, 0}, Acc);
checkForLinks("HTTP://" ++ Rest, Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "HTTP://", {link, 0}, Acc);
checkForLinks("Http://" ++ Rest, Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "Http://", {link, 0}, Acc);

checkForLinks([$(|Rest], Dir, Token, {link, Num}, Acc) ->
    checkForLinks(Rest, Dir, [Token, $(], {link, Num + 1}, Acc);

checkForLinks([$)|Rest], Dir, Token, {link, Num}, Acc) when Num > 0 ->
    checkForLinks(Rest, Dir, [Token, $)], {link, Num - 1}, Acc);

checkForLinks([Char|Rest], Dir, Token, {link, 0}, Acc) ->
    case lists:member(Char, ?ValidURLChars) of
        true ->
            checkForLinks(Rest, Dir, [Token, Char], {link, 0}, Acc);
        false ->
            case checkToken(Token, Char) of
                {true, Link} ->
                    html(lists:flatten(Acc), Dir, []) ++ Link ++
                        checkForLinks(Rest, Dir, "", text, [Char]);
                false ->
                    checkForLinks(Rest, Dir, "", text, [Acc, [Token, Char]])
            end
    end;

checkForLinks([Char|Rest], Dir, Token, {link, Num}, Acc) ->
    case lists:member(Char, ?ValidURLChars) of
        true ->
            checkForLinks(Rest, Dir, [Token, Char], {link, Num}, Acc);
        false ->
            checkForLinks(Rest, Dir, "", text, [Acc, [Token, Char]])
    end;

checkForLinks([Char|Rest], Dir, _Token, text, Acc) ->
    checkForLinks(Rest, Dir, "", text, [Acc, Char]).


checkToken(Token, Char) when (Char == $>)    or
                             (Char == $))    or
                             (Char == 9)     or
                             (Char == 10)    or
                             (Char == 11)    or
                             (Char == 12)    or
                             (Char == 13)    or
                             (Char == 32)    or
                             (Char == 133)   or
                             (Char == 160)   or
                             (Char == 5760)  or
                             (Char == 8232)  or
                             (Char == 8233)  or
                             (Char == 8239)  or
                             (Char == 8287)  or
                             (Char == 12288) or
                             ((Char > 8191) and (Char < 8203)) ->
    Link = lists:flatten(Token),
    case catch uri_string:parse(Link) of
        #{host   := Host,
          scheme := "https"} when (Host =/= "") ->
              {true, "<a href='" ++ Link ++ "'>" ++ Link ++ "</a>"};
        _ ->
            false
    end;
checkToken(_Token, _Char) ->
    false.

%% Base case
html("", _Dir, SoFar) -> lists:flatten(SoFar);

%% Row
html([13, 10 | Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, "<br />"]);
html([10     | Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, "<br />"]);
html([8232   | Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, "<br />"]);

%% -
html([8211 | Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, "-"]);

%% <3
html([$<, $3 | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, heart(Dir, [$<, $3])]);

%% :-) :) =) :] :>
html([$:, $-, $) | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, happy(Dir, [$:, $-, $)])]);
html([$:, $) | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, happy(Dir, [$:, $)])]);
html([$=, $) | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, happy(Dir, [$=, $)])]);
html([$:, $] | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, happy(Dir, [$:, $]])]);
html([$:, $> | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, happy(Dir, [$:, $>])]);

%% :-(  :(  =(  :< :[ :C
html([$:, $-, $( | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, sad(Dir, [$:, $-, $(])]);
html([$:, $( | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, sad(Dir, [$:, $(])]);
html([$=, $( | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, sad(Dir, [$=, $(])]);
html([$:, $< | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, sad(Dir, [$:, $<])]);
html([$:, $[ | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, sad(Dir, [$:, $[])]);
html([$:, $C | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, sad(Dir, [$:, $[])]);

%% ;-)  ;)  ^.~
html([$;, $-, $) | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, wink(Dir, [$;, $-, $)])]);
html([$;, $) | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, wink(Dir, [$;, $)])]);
html([$^, $., $~ | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, wink(Dir, [$^, $., $~])]);

%% =D :D
html([$=, $D | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, lol(Dir, [$=, $D])]);
html([$:, $D | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, lol(Dir, [$:, $D])]);

%% =O   :O :-O
html([$=, $O | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, shocked(Dir, [$=, $O])]);
html([$:, $O | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, shocked(Dir, [$:, $O])]);
html([$:, $-, $O | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, shocked(Dir, [$:, $-, $O])]);

%% =P   :P
html([$=, $P | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, mischief(Dir, [$=, $P])]);
html([$:, $P | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, mischief(Dir, [$:, $P])]);

%% :,(
html([$:, $,, $( | Rest], Dir, SoFar) ->
    html(Rest, Dir, [SoFar, crying(Dir, [$:, $,, $(])]);

html([229 | Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&aring;"]);
html([228 | Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&auml;"]);
html([246 | Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&ouml;"]);
html([197 | Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&Aring;"]);
html([196 | Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&Auml;"]);
html([214 | Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&Ouml;"]);
html([$< | Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&lt;"]);
html([$> | Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&gt;"]);
html([$& | Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&amp;"]);
html([$" | Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&quot;"]);
html([Char | Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, Char]).

heart(Dir, Txt)    -> smileHdr(Dir, Txt) ++ "heart.png' align=bottom>&nbsp;".
happy(Dir, Txt)    -> smileHdr(Dir, Txt) ++ "happy.png' align=bottom>&nbsp;".
sad(Dir, Txt)      -> smileHdr(Dir, Txt) ++ "sad.png' align=bottom>&nbsp;".
wink(Dir, Txt)     -> smileHdr(Dir, Txt) ++ "wink.png' align=bottom>&nbsp;".
lol(Dir, Txt)      -> smileHdr(Dir, Txt) ++ "lol.png' align=bottom>&nbsp;".
shocked(Dir, Txt)  -> smileHdr(Dir, Txt) ++ "shocked.png' align=bottom>&nbsp;".
crying(Dir, Txt)   -> smileHdr(Dir, Txt) ++ "crying.png' align=bottom>&nbsp;".
mischief(Dir, Txt) -> smileHdr(Dir, Txt) ++ "mischief.png' align=bottom>&nbsp;".

smileHdr(Dir, Txt) ->
    "&nbsp;<img class='emote' alt='" ++ Txt ++ "' src='" ++ Dir ++ "/Icons/".




%%======================================================================
%% Function : makeTimeLogReport(User, Rows) -> Html
%% Purpose  : Make time html report.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeTimeLogReport(_User, Rows, AllTask) ->
    Uids1 = [ETodo#etodo.uid || ETodo <- eRows:toList(Rows)],
    Uids2 = case AllTask of
                true ->
                    Uids1; %% Subtodos all ready in list.
                false ->
                    addSubTodos(Uids1)
            end,
    Uids3 = removeEmptyAndDone(Uids2),
    Opts = [{width, "20%"}, {align, center}],
    {EstimateSum, SpentSum, RemainingSum} = sum(Uids3),
    tableTag([makeTimeLogReport2(Uids3, []),
              trTag([{bgcolor, "black"}],
                    [tdTag([{width, "40%"}], heading("Total")),
                     tdTag(Opts, heading(EstimateSum)),
                     tdTag(Opts, heading(SpentSum)),
                     tdTag(Opts, heading(RemainingSum))
                    ])]).

makeTimeLogReport2([Uid | Rest], Acc) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    AllLogged = eTodoDB:getAllLoggedWork(Uid),
    Todo = eTodoDB:getTodo(tryInt(Uid)),
    Desc1 = eTodoDB:getWorkDesc(Uid),
    Desc2 = eGuiFunctions:getWorkDesc(Desc1, Todo#todo.description),
    Odd = ((length(Acc) rem 2) =/= 0),
    Opts2 = [{width, "20%"}, {align, center}],
    Opts3 = [{width, "20%"}],
    UidInt = tryInt(Uid),
    UidStr = eTodoUtils:convertUid(UidInt),
    Date = date(),

    Link1 = aTag([{href, convertUid(UidInt, Date)}], time(Estimate) ++ ":00"),
    Link2 = aTag([{href, convertUid(UidInt, incDate(Date, 1))}], AllLogged),
    Link3 = aTag([{href, convertUid(UidInt, incDate(Date, 2))}], time(Remaining) ++ ":00"),

    makeTimeLogReport2(Rest,
                       [trTag(bgColor(Odd),
                              [tdTag(Opts3, [aTag([{href, UidStr}], Desc2)]),
                               tdTag(Opts2, Link1),
                               tdTag(Opts2, Link2),
                               tdTag(Opts2, Link3)]) | Acc]);
makeTimeLogReport2([], Result) ->
    Opts = [{width, "20%"}, {align, center}],
    [trTag([{bgcolor, "black"}],
           [tdTag([{width, "40%"}], heading("Task description")),
            tdTag(Opts, heading("Time estimate(h)")),
            tdTag(Opts, heading("Time spent(h)")),
            tdTag(Opts, heading("Time left(h)"))
           ]) | lists:reverse(Result)].




removeEmptyAndDone(Uids) ->
    removeEmptyAndDone(Uids, []).

removeEmptyAndDone([], Acc) ->
    lists:reverse(Acc);
removeEmptyAndDone([Uid | Rest], Acc) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    AllLogged = eTodoDB:getAllLoggedWork(Uid),
    {ok, _Desc, _ShowInWL, ShowInTL} = eTodoDB:getWorkDescAll(Uid),
    UidInt = tryInt(Uid),
    Todo = eTodoDB:getTodo(UidInt),
    Done = (Todo#todo.status == done),
    case {{Estimate, Remaining, AllLogged}, default(ShowInTL, false), Done} of
        {{0, 0, "00:00"}, false, _Done} ->
            removeEmptyAndDone(Rest, Acc);
        {_Time, false, true} ->
            removeEmptyAndDone(Rest, Acc);
        _ ->
            removeEmptyAndDone(Rest, [Uid | Acc])
    end.


sum(Uids) ->
    sum(Uids, {0, {0, 0}, 0}).

sum([], {Est, {Hours, Min}, Rem}) ->
    SumHours = Hours + (Min div 60),
    SumMinutes = Min rem 60,
    {time(Est) ++ ":00",
     time(SumHours) ++ ":" ++ time(SumMinutes),
     time(Rem) ++ ":00"};
sum([Uid | Rest], {Est, {Hours, Min}, Rem}) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    {WHours, WMin} = eTodoDB:getAllLoggedWorkInt(Uid),
    sum(Rest, {Est + Estimate, {WHours + Hours, WMin + Min}, Rem + Remaining}).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
addSubTodos(Uids) ->
    addSubTodos2(Uids, []).



addSubTodos2([], Acc) ->
    lists:reverse(Acc);
addSubTodos2([Uid | Rest], Acc) ->
    SUids = addSubTodos(eTodoDB:getSubTodos(tryInt(Uid))),
    Acc2 = addToAccNoDuplicate([Uid | SUids], Acc),
    addSubTodos2(Rest, Acc2).

addToAccNoDuplicate([], Acc) ->
    Acc;
addToAccNoDuplicate([Uid | Rest], Acc) ->
    addToAccNoDuplicate(Rest, [toStr(Uid) | lists:delete(Uid, Acc)]).



%%======================================================================
%% Function : makeSceduleReport(User) -> Html
%% Purpose  : Make scedule html report.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeScheduleReport(User) ->
    Opts = [{width, "25%"}, {align, center}],
    Opts2 = [{width, "20%"}, {align, center}],
    Alarms = getAlarmList(User),
    ETodos = getTodoInfo(User),
    TimeMarkers = getTimeMarkers(),
    Events = lists:reverse(lists:keysort(1, TimeMarkers ++ Alarms ++ ETodos)),
    tableTag([trTag([{bgcolor, "black"}],
                    [tdTag([{width, "35%"}], heading("Description")),
                     tdTag(Opts, heading("Next alarm")),
                     tdTag(Opts2, heading("Due date")),
                     tdTag(Opts2, heading("Time left(h)"))]) |
              makeScheduleReport2(Events, [])]).

getTodoInfo(User) ->
    TodoList = eTodoDB:getTodos(User, ?defTaskList),
    getTodoInfo(TodoList, date(), []).

getTodoInfo([], _Now, Acc) ->
    Acc;
getTodoInfo([#todo{status = done} | Rest], Now, Acc) ->
    getTodoInfo(Rest, Now, Acc);
getTodoInfo([#todo{dueTime = undefined} | Rest], Now, Acc) ->
    getTodoInfo(Rest, Now, Acc);
getTodoInfo([#todo{uid = Uid, dueTime = DueDate} | Rest], Now, Acc) ->
    NextAlarm = {DueDate, {0, 0, 0}},
    {Desc, DueDate, UidStr, RemTime} = doSaveAlarmInfo(Uid),
    getTodoInfo(Rest, Now, [{NextAlarm, "", DueDate,
                             UidStr, Desc, RemTime} | Acc]);
getTodoInfo([_Todo | Rest], Now, Acc) ->
    getTodoInfo(Rest, Now, Acc).

getAlarmList(User) ->
    AlarmList = eTodoDB:getReminders(User),
    getAlarmInfo(AlarmList, date(), []).

getAlarmInfo([], _Now, Acc) ->
    Acc;
%% New alarm
getAlarmInfo([#alarmCfg{startDate = StartDate,
                        startTime = StartTime,
                        uid = Uid,
                        nextAlarm = undefined} | Rest], Now, Acc)
  when Now =< StartDate ->
    NextAlarm = {StartDate, repairTime(StartTime)},
    saveAlarmInfo(Uid, Rest, Now, NextAlarm, Acc);
%% Alarm that never will be activated
getAlarmInfo([#alarmCfg{nextAlarm = never} | Rest], Now, Acc) ->
    getAlarmInfo(Rest, Now, Acc);
getAlarmInfo([#alarmCfg{uid = Uid, nextAlarm = {Date, Time}} | Rest], Now, Acc)
  when Now =< Date ->
    saveAlarmInfo(Uid, Rest, Now, {Date, Time}, Acc);
getAlarmInfo([_Alarm | Rest], Now, Acc) ->
    getAlarmInfo(Rest, Now, Acc).

saveAlarmInfo(Uid, Rest, Now, NextAlarm, Acc) ->
    case doSaveAlarmInfo(Uid) of
        {Desc, DueDate, UidStr, RemTime} ->
            getAlarmInfo(Rest, Now, [{NextAlarm, NextAlarm, DueDate,
                                      UidStr, Desc, RemTime} | Acc]);
        _ ->
            %% Inconsistent data, throw it away.
            getAlarmInfo(Rest, Now, Acc)
    end.

doSaveAlarmInfo(Uid) ->
    case eTodoDB:getTodo(tryInt(Uid)) of
        Todo when is_record(Todo, todo) ->
            Desc1 = eTodoDB:getWorkDesc(toStr(Uid)),
            Desc2 = eGuiFunctions:getWorkDesc(Desc1, Todo#todo.description),
            DueDate = Todo#todo.dueTime,
            UidStr = eTodoUtils:convertUid(tryInt(Uid)),
            {_, RemTime} = eTodoDB:getTime(toStr(Uid)),
            {Desc2, DueDate, UidStr, RemTime};
        _ ->
            {error, inconsitentDatabase}
    end.

makeScheduleReport2([], Acc) ->
    Acc;
makeScheduleReport2([{_Key, Desc} | Rest], Acc) ->
    Odd = ((length(Rest) rem 2) =/= 0),
    makeScheduleReport2(Rest, [trTag(bgColor(Odd),
                                     [tdTag([{width, "35%"}], bTag(Desc)),
                                      tdTag([{width, "25%"}], ""),
                                      tdTag([{width, "20%"}], ""),
                                      tdTag([{width, "20%"}], "")]) | Acc]);
makeScheduleReport2([{_Key, DateTime, DueDate,
                      UidStr, Desc, RemTime} | Rest], Acc) ->
    Odd = ((length(Rest) rem 2) =/= 0),
    makeScheduleReport2(Rest, [trTag(bgColor(Odd),
                                     [tdTag([{width, "35%"}],
                                            [aTag([{href, UidStr}], Desc)]),
                                      tdTag([{width, "25%"},
                                             {align, "center"}],
                                            toStr(DateTime)),
                                      tdTag([{width, "20%"},
                                             {align, "center"}],
                                            toStr(DueDate)),
                                      tdTag([{width, "20%"},
                                             {align, "center"}],
                                            toStr(RemTime))]) | Acc]).

getTimeMarkers() ->
    Date = date(),
    Time = {0, 0, -1},
    [{{Time, Time}, "Overdue"},
     {{Date, Time}, "Today"},
     {{incDate(Date, 1), Time}, "Tomorrow"},
     {{incDate(Date, 2), Time}, getWeekDay(incDate(Date, 2))},
     {{incDate(Date, 3), Time}, getWeekDay(incDate(Date, 3))},
     {{incDate(Date, 4), Time}, getWeekDay(incDate(Date, 4))},
     {{incDate(Date, 5), Time}, getWeekDay(incDate(Date, 5))},
     {{incDate(Date, 6), Time}, getWeekDay(incDate(Date, 6))},
     {{incDate(Date, 7), Time}, "Week(s) away"},
     {{incDate(Date, 31), Time}, "Month(s) away"}].

repairTime({H, M}) -> {H, M, 0};
repairTime(undefined) -> {0, 0, 0};
repairTime(Time) -> Time.


