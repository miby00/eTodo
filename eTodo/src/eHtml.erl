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
-export([createSendMsg/2,
         pageHeader/1,
         pageHeader/2,
         pageFooter/0,
         generateMsg/4,
         generateSystemMsg/2,
         generateAlarmMsg/2,
         printTaskList/5,
         makeTaskList/5,
         makeHtmlTaskCSS/1,
         makeHtmlTaskCSS2/1,
         makeForm/2,
         makeHtml/1,
         makeWorkLogReport/2,
         makeTimeLogReport/3,
         makeSceduleReport/1,
         createTaskForm/1,
         showStatus/3,
         showLoggedWork/2,
         showTimeReport/1,
         showTimeReport/3]).

-export([htmlTag/0,   htmlTag/1,   htmlTag/2,
         headTag/0,   headTag/1,   headTag/2,
         bodyTag/0,   bodyTag/1,   bodyTag/2,
         titleTag/1,  titleTag/2,
         styleTag/1,  styleTag/2,
         tableTag/0,  tableTag/1,  tableTag/2,
         divTag/0,    divTag/1,    divTag/2,
         fontTag/0,   fontTag/1,   fontTag/2,
         pTag/0,      pTag/1,      pTag/2,
         bTag/0,      bTag/1,      bTag/2,
         tdTag/0,     tdTag/1,     tdTag/2,
         trTag/0,     trTag/1,     trTag/2,
         brTag/0,
         formTag/0,   formTag/1,   formTag/2,
         aTag/0,      aTag/1,      aTag/2,
         selectTag/1, selectTag/2, inputTag/1,
         metaTag/1,   imgTag/1]).

-include("eTodo.hrl").
-include_lib("inets/include/httpd.hrl").

-define(DodgerBlue, "#1e90ff").

-import(eTodoUtils, [toStr/1, toStr/2, tryInt/1, getWeekDay/1,
                     makeStr/1, getRootDir/0, dateTime/0,
                     convertUid/1, convertUid/2]).

%%%=====================================================================
%%% API
%%%=====================================================================

%%======================================================================
%% Make functions of HTML tags so we can produce html by calling them.
%%======================================================================

htmlTag()               -> tag(html, [],   []).
htmlTag(Content)        -> tag(html, [],   Content).
htmlTag(Attr, Content)  -> tag(html, Attr, Content).

headTag()               -> tag(head, [],   []).
headTag(Content)        -> tag(head, [],   Content).
headTag(Attr, Content)  -> tag(head, Attr, Content).

bodyTag()               -> tag(body, [],   []).
bodyTag(Content)        -> tag(body, [],   Content).
bodyTag(Attr, Content)  -> tag(body, Attr, Content).

titleTag(Content)       -> tag(title, [],   Content).
titleTag(Attr, Content) -> tag(title, Attr, Content).

styleTag(Content)       -> tag(style, [],   Content).
styleTag(Attr, Content) -> tag(style, Attr, Content).

tableTag()              -> tag(table, [],   []).
tableTag(Content)       -> tag(table, [],   Content).
tableTag(Attr, Content) -> tag(table, Attr, Content).

divTag()               -> tag('div', [],   []).
divTag(Content)        -> tag('div', [],   Content).
divTag(Attr, Content)  -> tag('div', Attr, Content).

fontTag()               -> tag(font,  [],   []).
fontTag(Content)        -> tag(font,  [],   Content).
fontTag(Attr, Content)  -> tag(font,  Attr, Content).

aTag()                  -> tag(a,     [],   []).
aTag(Content)           -> tag(a,     [],   Content).
aTag(Attr, Content)     -> tag(a,     Attr, Content).

pTag()                  -> tag(p,     [],   []).
pTag(Content)           -> tag(p,     [],   Content).
pTag(Attr, Content)     -> tag(p,     Attr, Content).

bTag()                  -> tag(b,     [],   []).
bTag(Content)           -> tag(b,     [],   Content).
bTag(Attr, Content)     -> tag(b,     Attr, Content).

trTag()                 -> tag(tr,    [],   []).
trTag(Content)          -> tag(tr,    [],   Content).
trTag(Attr, Content)    -> tag(tr,    Attr, Content).

tdTag()                 -> tag(td,    [],   []).
tdTag(Content)          -> tag(td,    [],   Content).
tdTag(Attr, Content)    -> tag(td,    Attr, Content).

formTag()               -> tag(form,  [],   []).
formTag(Content)        -> tag(form,  [],   Content).
formTag(Attr, Content)  -> tag(form,  Attr, Content).

selectTag(Content)      -> tag(select,  [],   Content).
selectTag(Attr, Content)-> tag(select,  Attr, Content).

inputTag(Attr)          -> cTag(input,  Attr).

imgTag(Attr)            -> cTag(img,  Attr).

metaTag(Attr)           -> cTag(meta, Attr).

brTag()                 -> cTag(br, []).

tag(Tag, Attr, Content)  ->
    HTag  = [$<, toStr(Tag), attr(Attr), $>, Content, $<, $/, toStr(Tag), $>],
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
attr([{nonEmpty, _}|Rest], Acc) ->
    attr(Rest, Acc);
attr([{Name, Value}|Rest], Acc) when is_integer(Value) ->
    attr(Rest, [Acc, 32, toStr(Name), $=, toStr(Value)]);
attr([{Name, Value}|Rest], Acc) ->
    attr(Rest, [Acc, 32, toStr(Name), $=, $', toStr(Value), $']);
attr([Name|Rest], Acc) ->
    attr([{Name, Name}|Rest], Acc).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
printTaskList(User, List, Filter, SearchText, Cfg) ->
    ETodos = eTodoDB:getETodos(User, List, Filter, SearchText, Cfg),
    Body   = [[makePrintTaskList(ETodo), pTag()] || ETodo <- ETodos],
    ["<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN' ",
     "'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>",
     htmlTag([{"xmlns",    "http://www.w3.org/1999/xhtml"},
              {"xml:lang", "en"},
              {"lang",     "en"}],
             [headTag([metaTag([{"http-equiv", "Content-Type"},
                                {content,      "text/html; charset=UTF-8"}]),
                       titleTag("eTodo - " ++ User)]),
              bodyTag(Body)])].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makePrintTaskList(#etodo{status      = Status,
                         priority    = Priority,
                         dueTime     = DueTime,
                         description = Description,
                         comment     = Comment,
                         sharedWith  = SharedWith,
                         createTime  = CreateTime,
                         doneTime    = DoneTime,
                         progress    = Progress,
                         owner       = Owner,
                         uid         = Uid}) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    AllLoggedWork         = eTodoDB:getAllLoggedWork(Uid),

    [tableTag([{width, "100%"}, {bgcolor, "#b9c9fe"}],
              [trTag([headerCell(?status),
                      dataCell(Status, [{width, "20%"}]),
                      headerCell(?prio),
                      dataCell(Priority, [{width, "20%"}]),
                      headerCell("Progress(%)"),
                      dataCell(toStr(Progress), [{width, "20%"}])]),
               trTag([headerCell(?createTime),
                      dataCell(CreateTime, [{width, "20%"}]),
                      headerCell(?dueTime),
                      dataCell(DueTime, [{width, "20%"}]),
                      headerCell(?doneTimestamp),
                      dataCell(DoneTime, [{width, "20%"}])]),
               trTag([headerCell("Estimate(h)"),
                      dataCell(toStr(Estimate), [{width, "20%"}]),
                      headerCell("Logged(h)"),
                      dataCell(AllLoggedWork, [{width, "20%"}]),
                      headerCell("Remaining(h)"),
                      dataCell(toStr(Remaining), [{width, "20%"}])]),
               trTag([headerCell(?owner),
                      dataCell(Owner, [{width, "20%"}]),
                      headerCell(?sharedWith),
                      dataCell(SharedWith, [{colspan, 3}])]),
               trTag([{nonEmpty, Description}],
                     [headerCell(?description),
                      dataCell(Description, [{colspan, 5}])]),
               trTag([{nonEmpty, Comment}],
                     [headerCell(?comment),
                      dataCell(Comment, [{colspan, 5}])])])].

headerCell(Text) ->
    tdTag([{width, "13%"}, {bgcolor, "#b9c9fe"}],
          fontTag([{size, 1}], bTag([Text, $:]))).

dataCell(Text, Extra) ->
    tdTag([{bgcolor, "#e8edff"}|Extra], fontTag([{size, 1}], makeHtml(Text))).

%%======================================================================
%% Function : makeWorkLogReport(User, Date) -> Html
%% Purpose  : Make html report of work log.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeWorkLogReport(User, Date) ->
    {D1, D2, D3, D4, D5, D6, D7, Act4} = calcReport(User, Date),
    Opts = [{width, "11%"}, {align, center}],
    tableTag([
              makeWorkLogReport(Date, Act4, {D1, D2, D3, D4, D5, D6, D7}, []),
              trTag([{bgcolor, "black"}],
                    [tdTag([{width, "23%"}], heading("Total")),
                     tdTag(Opts, heading(tot(D1))),
                     tdTag(Opts, heading(tot(D2))),
                     tdTag(Opts, heading(tot(D3))),
                     tdTag(Opts, heading(tot(D4))),
                     tdTag(Opts, heading(tot(D5))),
                     tdTag(Opts, heading(tot(D6))),
                     tdTag(Opts, heading(tot(D7)))
                    ])]).

showLoggedWork(User, Date) ->
    {D1, D2, D3, D4, D5, D6, D7, Act4} = calcReport(User, Date),
    tableTag([{class, "workLogTable"}],
             [showLoggedWork(Date, Act4, {D1, D2, D3, D4, D5, D6, D7}, []),
              trTag([{class, "workLogSum"}],
                    [tdTag([{class, "workLogDesc workLogSum"}], "Total"),
                     [tdTag([{class, "workLogSum workLogColBig"}], tot(D1)),
                      tdTag([{class, "workLogSum workLogColBig"}], tot(D2)),
                      tdTag([{class, "workLogSum workLogColBig"}], tot(D3)),
                      tdTag([{class, "workLogSum"}], tot(D4)),
                      tdTag([{class, "workLogSum"}], tot(D5)),
                      tdTag([{class, "workLogSum"}], tot(D6)),
                      tdTag([{class, "workLogSum"}], tot(D7))
                     ]])]).

calcReport(User, Date) ->
    D1 = eTodoDB:getLoggedWork(User, Date),
    D2 = eTodoDB:getLoggedWork(User, incDate(Date, 1)),
    D3 = eTodoDB:getLoggedWork(User, incDate(Date, 2)),
    D4 = eTodoDB:getLoggedWork(User, incDate(Date, 3)),
    D5 = eTodoDB:getLoggedWork(User, incDate(Date, 4)),
    D6 = eTodoDB:getLoggedWork(User, incDate(Date, 5)),
    D7 = eTodoDB:getLoggedWork(User, incDate(Date, 6)),
    All = lists:flatten([D1, D2, D3, D4, D5, D6, D7]),
    Act1 = [Task || {Task, _H, _M} <- All],
    Act2 = lists:usort(Act1),
    Act3 = [{Task, eTodoDB:getWorkDesc(Task)} || Task <- Act2],
    Act4 = lists:keysort(2, Act3),
    {D1, D2, D3, D4, D5, D6, D7, Act4}.

tot(Days) ->
    tot(Days, {0, 0}).

tot([], {SumHours, SumMin}) ->
    SumHours2   = SumHours + (SumMin div 60),
    SumMinutes2 = SumMin rem 60,
    time(SumHours2) ++ ":" ++ time(SumMinutes2);
tot([{_, Hours, Min}|Rest], {SumHours, SumMin}) ->
    tot(Rest, {SumHours + Hours, SumMin + Min}).

incDate(Date, Inc) ->
    Days = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(Days + Inc).



showLoggedWork(Date, [], _Days, Result) ->
    Opts  = [{class, "workLogColumn"}],
    Opts2 = [{class, "workLogColumn workLogColBig"}],
    [trTag([{class, "workLogHeading"}],
           [tdTag([{class, "workLogDesc"}], "Task description"),
            tdTag(Opts2, getWeekDay(Date)),
            tdTag(Opts2, getWeekDay(incDate(Date, 1))),
            tdTag(Opts2, getWeekDay(incDate(Date, 2))),
            tdTag(Opts,  getWeekDay(incDate(Date, 3))),
            tdTag(Opts,  getWeekDay(incDate(Date, 4))),
            tdTag(Opts,  getWeekDay(incDate(Date, 5))),
            tdTag(Opts,  getWeekDay(incDate(Date, 6)))
           ])| lists:reverse(Result)];
showLoggedWork(Date, [{Act, Desc}|Rest],
               Days = {D1, D2, D3, D4, D5, D6, D7}, SoFar) ->
    Odd    = ((length(Rest) rem 2) == 0),
    UidStr = eTodoUtils:convertUid(list_to_integer(Act)),
    Opts   = if Odd  -> [{class, "lwOdd"}];
                true -> [{class, "lwEven"}]
             end,
    Opts2 = [{class, "workLogValue"}],
    Opts3 = [{class, "workLogValue workLogColBig"}],
    Row  = trTag(Opts,
                 [tdTag(aTag([{href, "/eTodo/eWeb:showTodo?uid=" ++
                                   http_uri:encode(UidStr)}], empty(Desc, Act))),
                  tdTag(Opts3, hours(Act, D1) ++ ":" ++ minutes(Act, D1)),
                  tdTag(Opts3, hours(Act, D2) ++ ":" ++ minutes(Act, D2)),
                  tdTag(Opts3, hours(Act, D3) ++ ":" ++ minutes(Act, D3)),
                  tdTag(Opts2, hours(Act, D4) ++ ":" ++ minutes(Act, D4)),
                  tdTag(Opts2, hours(Act, D5) ++ ":" ++ minutes(Act, D5)),
                  tdTag(Opts2, hours(Act, D6) ++ ":" ++ minutes(Act, D6)),
                  tdTag(Opts2, hours(Act, D7) ++ ":" ++ minutes(Act, D7))]),
    showLoggedWork(Date, Rest, Days, [Row|SoFar]).

makeWorkLogReport(Date, [], _Days, Result) ->
    Opts = [{width, "11%"}, {align, center}],
    [trTag([{bgcolor, "black"}],
           [tdTag([{width, "23%"}], heading("Task description")),
            tdTag(Opts, heading(getWeekDay(Date))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 1)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 2)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 3)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 4)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 5)))),
            tdTag(Opts, heading(getWeekDay(incDate(Date, 6))))
           ])| lists:reverse(Result)];
makeWorkLogReport(Date, [{Act, Desc}|Rest],
                  Days = {D1, D2, D3, D4, D5, D6, D7}, SoFar) ->
    Odd    = ((length(Rest) rem 2) == 0),
    Opts   = [{align, center}],
    Uid    = list_to_integer(Act),
    UidStr = convertUid(Uid),

    Link1  = aTag([{href, convertUid(Uid, Date)}], hours(Act, D1) ++ ":" ++ minutes(Act, D1)),
    Link2  = aTag([{href, convertUid(Uid, incDate(Date, 1))}], hours(Act, D2) ++ ":" ++ minutes(Act, D2)),
    Link3  = aTag([{href, convertUid(Uid, incDate(Date, 2))}], hours(Act, D3) ++ ":" ++ minutes(Act, D3)),
    Link4  = aTag([{href, convertUid(Uid, incDate(Date, 3))}], hours(Act, D4) ++ ":" ++ minutes(Act, D4)),
    Link5  = aTag([{href, convertUid(Uid, incDate(Date, 4))}], hours(Act, D5) ++ ":" ++ minutes(Act, D5)),
    Link6  = aTag([{href, convertUid(Uid, incDate(Date, 5))}], hours(Act, D6) ++ ":" ++ minutes(Act, D6)),
    Link7  = aTag([{href, convertUid(Uid, incDate(Date, 6))}], hours(Act, D7) ++ ":" ++ minutes(Act, D7)),

    Row  = trTag(bgColor(Odd),
                 [tdTag(aTag([{href, UidStr}], empty(Desc, Act))),
                  tdTag(Opts, Link1),
                  tdTag(Opts, Link2),
                  tdTag(Opts, Link3),
                  tdTag(Opts, Link4),
                  tdTag(Opts, Link5),
                  tdTag(Opts, Link6),
                  tdTag(Opts, Link7)]),
    makeWorkLogReport(Date, Rest, Days, [Row|SoFar]).

empty("", Value)        -> Value;
empty(undefined, Value) -> Value;
empty(Value, _)         -> makeHtml(Value).

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

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
pageHeader(UserName) ->
    pageHeader("", UserName).

pageHeader(Extra, UserName) ->
    StyleSheet = getStyleSheet(UserName),
    {ok, Styles} = file:read_file(StyleSheet),
    Content = "width=device-width; initial-scale=1.0; "
        "maximum-scale=1.0; user-scalable=0;",
    Font =  "<link rel='stylesheet' type='text/css' "
        "href='https://fonts.googleapis.com/css"
        "?family=Ubuntu:regular,bold&subset=Latin'>",
    ManifestFile = filename:join(["priv", "css", "manifest.json"]),
    Manifest     = "<link rel='manifest' href='/" ++ ManifestFile ++ "'>",
    IconFile     = filename:join(["priv", "Icons", "etodoSuper.png"]),
    Icon         = "<link rel='icon' sizes='192x192' href='/" ++ IconFile ++ "'>",
    ["<!DOCTYPE html><html>",
     [headTag(
        [titleTag(["eTodo - ", UserName]),
         metaTag([{name, "mobile-web-app-capable"}, {content, "yes"}]),
         metaTag([{content, Content}, {name, "viewport"}]),
         metaTag([{"http-equiv", "Content-Type"},
                  {content,      "text/html; charset=UTF-8"}]),

         Font, Manifest, Icon,
         styleTag([{type, "text/css"}], Styles),
         javascript()
        ]),
      "<body " ++ Extra ++ "><div id='container'>"]].

getStyleSheet(UserName) ->
    FileName = lists:concat(["styles_", UserName, ".css"]),
    FullFile = filename:join([getRootDir(), "www", "priv", "css", FileName]),
    case filelib:is_file(FullFile) of
        true ->
            FullFile;
        false ->
            filename:join([getRootDir(), "www", "priv", "css", "styles.css"])
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
javascript() ->
    "<script type='text/JavaScript' src='/priv/js/javascript.js'></script>".

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
pageFooter() ->
    "</div></body></html>".

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeForm(User, Default) ->
    [tableTag([{id, "tableHeader"}],
              [trTag([tdTag([{id, "toolbar"}],
                            [aTag([{id,   "createTodo"},
                                   {href, "/eTodo/eWeb:createTodo"}],
                                  [imgTag([{src, "/priv/Icons/createNew.png"},
                                           {alt, "Create new"}])]),
                             aTag([{id,   "message"},
                                   {href, "/eTodo/eWeb:message"}],
                                  [imgTag([{src, "/priv/Icons/message.png"},
                                           {alt, "Messages"}])])]),
                      tdTag(createForm(User, Default))])])].

createForm(User, Default) ->
    TodoLists =
        case eTodoDB:readUserCfg(User) of
            #userCfg{lists = undefined} ->
                [?defLoggedWork, ?defTimeReport, ?defShowStatus,
                  ?defInbox, ?defTaskList];
            #userCfg{lists = Lists} ->
                [?defLoggedWork, ?defTimeReport, ?defShowStatus, ?defInbox|Lists]
        end,
    formTag([{action, "/eTodo/eWeb:listTodos"},
             {'accept-charset', "UTF-8"},
             {id, "searchForm"},
             {method, "get"}],
            [selectTag([{id,       "taskSelect"},
                        {name,     "list"},
                        {onchange, "submitForm()"}],
                       createForm2(TodoLists, Default)),
             inputTag([{type,    "submit"},
                       {id,      "searchButton"},
                       {onclick, "submitForm()"},
                       {value,   "Search"}]),
             inputTag([{type,  "text"},
                       {name,  "search"},
                       {id,    "searchField"},
                       {class, "rounded"}])]).

createForm2(List, Default) ->
    createForm(lists:reverse(List), [], Default).

createForm([], Result, _Default) ->
    Result;
createForm([Value|Rest], SoFar, Value) ->
    Value2 = unicode:characters_to_binary(Value, utf8),
    Option = ["<option value='", Value2, "' selected='selected'>",
              Value2, "</option>\r\n"],
    createForm(Rest, [Option, SoFar], Value);
createForm([Value|Rest], SoFar, Default) ->
    Value2 = unicode:characters_to_binary(Value, utf8),
    Option = ["<option value='", Value2, "'>", Value2, "</option>\r\n"],
    createForm(Rest, [Option, SoFar], Default).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
createTaskForm(User) ->
    TaskLists = case eTodoDB:readUserCfg(User) of
                    #userCfg{lists = undefined} ->
                        [?defTaskList];
                    #userCfg{lists = Lists} ->
                        Lists
                end,
    [formTag([{action,           "/eTodo/eWeb:createTask"},
              {'accept-charset', "UTF-8"},
              {method,           "get"}],
             [tableTag([{id, "createTable"}],
                       [trTag(
                          [tdTag([{class, "description"}],"Set task list:"),
                           tdTag([{class, "value"}],
                                 [selectTag([{class, "selects"},
                                             {name,  "list"}],
                                            createForm2(TaskLists,
                                                        ?defTaskList))]),
                           tdTag(),
                           tdTag()]),
                        trTag(
                          [tdTag([{class, "description"}], "Set status:"),
                           tdTag([{class, "value"}],
                                 [selectTag([{class, "selects"},
                                             {name, "status"}],
                                            createForm2([?descPlanning,
                                                         ?descInProgress,
                                                         ?descDone,
                                                         ?descNA],
                                                        ?descNA)
                                           )]),
                           tdTag([{class, "description"}], "Set prio:"),
                           tdTag([{class, "value"}],
                                 selectTag([{class, "selects"},
                                            {name,  "prio"}],
                                           createForm2([?descLow,
                                                        ?descMedium,
                                                        ?descHigh,
                                                        ?descNA],
                                                       ?descNA)))]),
                        trTag(
                          [tdTag([{class, "description"}], "Description:"),
                           tdTag([{class,   "value"},
                                  {colspan, 3}],
                                 inputTag([{class, "textField"},
                                           {type,  "text"},
                                           {name,  "desc"}]))]),
                        trTag(
                          [tdTag([{class, "description"}], "Comment:"),
                           tdTag([{class,   "value"},
                                  {colspan, 3}],
                                 inputTag([{class, "textField"},
                                           {type,  "text"},
                                           {name,  "comment"}]))]),
                        trTag(
                          [tdTag([{colspan, 4}],
                                 inputTag([{type,  "submit"},
                                           {name,  "submit"},
                                           {id,    "createTask"},
                                           {value, "Create task"}]))])])])].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeTaskList(User, List, Filter, SearchText, Cfg) ->
    ETodos = eTodoDB:getETodos(User, List, Filter, SearchText, Cfg),
    [makeHtmlTaskCSS(ETodo) || ETodo <- ETodos].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeHtmlTaskCSS(ETodo = #etodo {hasSubTodo = true}) ->
    makeHtmlTaskCSS2(ETodo);
makeHtmlTaskCSS(#etodo{uid         = Uid,
                       status      = Status,
                       priority    = Priority,
                       dueTime     = DueTime,
                       description = Description,
                       comment     = Comment,
                       sharedWith  = SharedWith,
                       createTime  = CreateTime,
                       doneTime    = DoneTime,
                       progress    = Progress,
                       owner       = Owner,
                       hasSubTodo  = HasSubTodo}) ->
    ProgStr = toStr(Progress),
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    [makeTableHeader(Uid, HasSubTodo), "<tr>",
     headerCellCSS(?status), createStatusDataCell(Status, Uid),
     headerCellCSS(?prio),   dataCellCSS2(Priority, ""),
     "</tr><tr>",
     headerCellCSS(?sharedWith), dataCellCSS(SharedWith, ""),
     headerCellCSS(?owner),      dataCellCSS(Owner,      ""),
     "</tr><tr class='hideRow'>",
     headerCellCSS(?createTime), dataCellCSS2(CreateTime, ""),
     headerCellCSS(?dueTime),    dataCellCSS2(DueTime,    ""),
     "</tr><tr class='hideRow'>",
     headerCellCSS(?doneTimestamp), dataCellCSS(DoneTime, ""),
     headerCellCSS("Progress(%)"),  dataCellCSS2(ProgStr, ""),
     "</tr><tr class='hideRow'>",
     headerCellCSS("Estimate(h)"),  dataCellCSS(toStr(Estimate),   ""),
     headerCellCSS("Remaining(h)"), dataCellCSS2(toStr(Remaining), ""),
     "</tr><tr>",
     headerCellCSS(?description), dataCellCSS(Description, "colspan=3"),
     "</tr><tr class='hideRow'>",
     headerCellCSS(?comment), dataCellCSS(Comment, "colspan=3"),
     "</tr></table>"].

makeHtmlTaskCSS2(#etodo{uid         = Uid,
                        status      = Status,
                        priority    = Priority,
                        dueTime     = DueTime,
                        description = Description,
                        comment     = Comment,
                        sharedWith  = SharedWith,
                        createTime  = CreateTime,
                        doneTime    = DoneTime,
                        progress    = Progress,
                        owner       = Owner,
                        hasSubTodo  = HasSubTodo}) ->
    ProgStr = toStr(Progress),
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    [makeTableHeader(Uid, HasSubTodo), "<tr>",
     headerCellCSS(?status), dataCellCSS2(Status,   ""),
     headerCellCSS(?prio),   dataCellCSS2(Priority, ""),
     "</tr><tr>",
     headerCellCSS(?sharedWith), dataCellCSS(SharedWith, ""),
     headerCellCSS(?owner),      dataCellCSS(Owner,      ""),
     "</tr><tr>",
     headerCellCSS(?createTime), dataCellCSS2(CreateTime, ""),
     headerCellCSS(?dueTime),    dataCellCSS2(DueTime,    ""),
     "</tr><tr>",
     headerCellCSS(?doneTimestamp), dataCellCSS(DoneTime, ""),
     headerCellCSS("Progress(%)"),  dataCellCSS2(ProgStr, ""),
     "</tr><tr>",
     headerCellCSS("Estimate(h)"),  dataCellCSS(toStr(Estimate),   ""),
     headerCellCSS("Remaining(h)"), dataCellCSS2(toStr(Remaining), ""),
     "</tr><tr>",
     headerCellCSS(?description), dataCellCSS(Description, "colspan=3"),
     nonEmpty(Comment, ["</tr><tr>",
                        headerCellCSS(?comment),
                        dataCellCSS(Comment, "colspan=3")]),
     "</tr></table>"].

nonEmpty("", _) ->
    "";
nonEmpty(undefined, _) ->
    "";
nonEmpty(_, Value) ->
    Value.

makeTableHeader(Uid, false) ->
    ["<table class='todoTable tCompact' id='table", Uid, "' ",
     "OnClick=\"showDetails(", Uid, ");\">"];
makeTableHeader(Uid, true) ->
    ["<table class='subTodoTable' id='table", Uid, "' "
     "OnClick=\"openInNewTab('/eTodo/eWeb:listTodos",
     "?list=", Uid, "&search=&submit=Search');\">"].

headerCellCSS(Text) ->
    ["<td class=header>", Text, ":</td>\r\n"].

dataCellCSS(Text, Extra) ->
    ["<td class=data ", Extra, ">", makeHtml(Text, "/priv"), "</td>\r\n"].

dataCellCSS2(Text, Extra) ->
    ["<td class=data2 ", Extra, ">", makeHtml(Text, "/priv"), "</td>\r\n"].

createStatusDataCell(Status, Uid) ->
    ["<td><select name='status' id='id", Uid, "' ",
     "OnChange=\"sendStatus('id", Uid, "', '", Uid, "');\" ",
     "OnClick=\"event.cancelBubble = true;\">\r\n",
     createForm2([?descPlanning, ?descInProgress, ?descDone, ?descNA], Status),
     "</select></td>"].

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
    {tableTag([{align, "right"}, {width, "100%"}],
              [trTag(
                 [tdTag([{width, "150"}], []),
                  tdTag([{valign, "top"}, {align, "left"}, {width, "100%"}],
                        fontTag([{color, "Blue"}], [Sender, UserList])),
                  tdTag([{valign, "top"}, {width, "80"}, {align, "center"}],
                        fontTag([{color, ?DodgerBlue}], toStr(time(), time))),
                  tdTag([{valign, "top"}, {width, "40"}],
                        imgTag([{src, getRootDir() ++ "/Icons/" ++ Icon}]))]),
               trTag(
                 [tdTag(),
                  tdTag([{align, "left"}], makeHtml(Text)),
                  tdTag([{valign, "top"}, {align, center}],
                        imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                  tdTag()])]),
     tableTag([{class, "msgSent"}],
              [trTag(
                 [tdTag([{class, "msgText"}], [Sender, UserList]),
                  tdTag([{class, "msgTime"}], toStr(time(), time)),
                  tdTag([{class, "msgImg"}],
                        imgTag([{src, "/priv/Icons/" ++ Icon}]))
                 ]),
               trTag(
                 [tdTag(makeHtml(Text, "/priv")),
                  tdTag([{class, "portraitCol"}],
                        imgTag([{src, WPortrait}, {class, "portrait"}])),
                  tdTag()])])};
generateMsg(_User, Sender, Users, Text) ->
    Icon      = getIcon(Users),
    Portrait  = getPortrait(Sender),
    WPortrait = getWebPortrait(Sender),
    UserList  = case Users of
                    [] ->
                        "";
                    _ ->
                        [" to ", makeHtml(makeStr(Users))]
                end,
    {tableTag([{align, "left"}, {width, "100%"}],
              [trTag(
                 [tdTag([{valign, "top"}, {width, "40"}],
                        imgTag([{src, getRootDir() ++ "/Icons/" ++ Icon}])),
                  tdTag([{valign, "top"}, {width, "80"}, {align, "center"}],
                        fontTag([{color, ?DodgerBlue}], toStr(time(), time))),
                  tdTag([{valign, "top"}, {width, "100%"}],
                        fontTag([{color, "Blue"}], [Sender, UserList])),
                  tdTag([{width, "150"}], [])]),
               trTag(
                 [tdTag(),
                  tdTag([{valign, "top"}, {align, center}],
                        imgTag([{src, Portrait}, {height, "45"}, {width, "45"}])),
                  tdTag(makeHtml(Text)), tdTag()])]),
     tableTag([{class, "msgReceived"}],
              [trTag(
                 [tdTag([{class, "msgImg"}],
                        imgTag([{src, "/priv/Icons/" ++ Icon}])),
                  tdTag([{class, "msgTime"}],
                        toStr(time(), time)),
                  tdTag([{class, "msgText"}], [Sender, UserList])]),
               trTag(
                 [tdTag(),
                  tdTag([{class, "portraitCol"}],
                        imgTag([{src, WPortrait}, {class, "portrait"}])),
                  tdTag(makeHtml(Text, "/priv"))])])}.

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
    {tableTag(
       [trTag(
          [tdTag([{valign, "top"}],
                 imgTag([{src, getRootDir() ++ "/Icons/etodoChat.png"}])),
           tdTag([{valign, "top"}],
                 fontTag([{color, ?DodgerBlue}],
                         toStr(time(), time))),
           tdTag([{valign, "top"}],
                 fontTag([{color, "Blue"}], "eTodo"))]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text))])]),
     tableTag(
       [trTag(
          [tdTag([{class, "msgImg"}],
                 imgTag([{src, "/priv/Icons/etodoChat.png"}])),
           tdTag([{class, "msgTime"}], toStr(time(), time)),
           tdTag([{class, "msgText"}],"eTodo")]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text, "/priv"))])])};
generateSystemMsg(Uid, Text) ->
    UidStr = eTodoUtils:convertUid(Uid),
    {tableTag(
       [trTag(
          [tdTag([{valign, "top"}],
                 imgTag([{src, getRootDir() ++ "/Icons/etodoChat.png"}])),
           tdTag([{valign, "top"}],
                 fontTag([{color, ?DodgerBlue}],
                         toStr(time(), time))),
           tdTag([{valign, "top"}],
                 fontTag([{color, "Red"}],
                         aTag([{href, UidStr}], "eTodo")))]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text))])]),
     tableTag(
       [trTag(
          [tdTag([{class, "msgImg"}],
                 imgTag([{src, "/priv/Icons/etodoChat.png"}])),
           tdTag([{class, "msgTime"}], toStr(time(), time)),
           tdTag([{class, "msgText"}],
                 aTag([{href, "/eTodo/eWeb:showTodo?uid=" ++
                            http_uri:encode(UidStr)}], "eTodo"))]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text, "/priv"))])])}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
generateAlarmMsg(timer, Text) ->
    {tableTag(
       [trTag(
          [tdTag([{valign, "top"}],
                 imgTag([{src, getRootDir() ++ "/Icons/clockChat.png"}])),
           tdTag([{valign, "top"}],
                 fontTag([{color, ?DodgerBlue}],
                         toStr(time(), time))),
           tdTag([{valign, "top"}],
                 fontTag([{color, "Red"}], "Timer expired"))]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text))])]),
     tableTag(
       [trTag(
          [tdTag([{class, "msgImg"}],
                 imgTag([{src, "/priv/Icons/clockChat.png"}])),
           tdTag([{class, "msgTime"}], toStr(time(), time)),
           tdTag([{class, "msgText"}], "Timer expired")]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text, "/priv"))])])};
generateAlarmMsg(Uid, Text) ->
    UidStr = eTodoUtils:convertUid(Uid),
    {tableTag(
       [trTag(
          [tdTag([{valign, "top"}],
                 imgTag([{src, getRootDir() ++ "/Icons/clockChat.png"}])),
           tdTag([{valign, "top"}],
                 fontTag([{color, ?DodgerBlue}],
                         toStr(time(), time))),
           tdTag([{valign, "top"}],
                 fontTag([{color, "Red"}],
                         aTag([{href, UidStr}], "eTodo")))]),
        trTag([tdTag(), tdTag(), tdTag(makeHtml(Text))])]),
     tableTag([{class, "msgAlarm"}],
              [trTag(
                 [tdTag([{class, "msgImg"}],
                        imgTag([{src, "/priv/Icons/clockChat.png"}])),
                  tdTag([{class, "msgTime"}], toStr(time(), time)),
                  tdTag([{class, "msgText"}],
                        aTag([{href, "/eTodo/eWeb:showTodo?uid=" ++
                                   http_uri:encode(UidStr)}], "eTodo"))]),
               trTag([tdTag(), tdTag(), tdTag(makeHtml(Text, "/priv"))])])}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
createSendMsg(Default, Users) ->
    UserList = ["All" | Users],
    Script   = "sendMsg(\"userSelect\", \"sendMessage\");",
    tableTag([{id, "footer"}],
             [trTag(
                [tdTag(
                   [selectTag([{id,   "userSelect"},
                               {name, "users"}],
                              createForm2(UserList, Default)),
                    inputTag([{type,      "submit"},
                              {name,      "submit"},
                              {id,        "sendButton"},
                              {value,     "Send"},
                              {"OnClick", Script}]),
                    inputTag([{type,      "text"},
                              {name,      "sendMsg"},
                              {onkeydown, "return checkForEnter(event);"},
                              {id,        "sendMessage"},
                              {class,     "rounded"}])])])]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

makeHtml(Text) ->
    makeHtml(Text, getRootDir()).

makeHtml(Text, Dir) ->
    unicode:characters_to_binary(checkForLinks(Text, Dir), utf8).

checkForLinks(Text, Dir) ->
    case catch re:run(Text, "https?://[^\s\t\r\n]*") of
        nomatch ->
            html(Text, Dir, []);
        {match, [{Begining, Length}]} ->
            Link = string:substr(Text, Begining + 1, Length),
            html(string:sub_string(Text, 1, Begining), Dir, []) ++
                "<a href='" ++ Link ++ "'>" ++ Link ++ "</a>" ++
                checkForLinks(string:substr(Text, Begining + Length + 1), Dir);
        _ ->
            html(Text, Dir, [])
    end.

%% Base case
html("", _Dir, SoFar) -> lists:flatten(SoFar);

%% Row
html([13, 10|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, "<br />"]);
html([10|Rest],     Dir, SoFar) -> html(Rest, Dir, [SoFar, "<br />"]);

%% -
html([8211|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, "-"]);

%% :-) :) =) :] :> C: (:
html([$:, $-, $)|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);
html([$:, $)    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);
html([$=, $)    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);
html([$:, $]    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);
html([$:, $>    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);
html([$C, $:    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);
html([$(, $:    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, happy(Dir)]);

%% :-(  :(  =(  :< :[
html([$:, $-, $(|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, sad(Dir)]);
html([$:, $(    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, sad(Dir)]);
html([$=, $(    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, sad(Dir)]);
html([$:, $<    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, sad(Dir)]);
html([$:, $[    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, sad(Dir)]);

%% ;-)  ;)  ^.~
html([$;, $-, $)|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, wink(Dir)]);
html([$;, $)    |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, wink(Dir)]);
html([$^, $., $~|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, wink(Dir)]);

%% =D :D
html([$=, $D |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, lol(Dir)]);
html([$:, $D |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, lol(Dir)]);

%% =O   :O   O:
html([$=, $O   |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, shocked(Dir)]);
html([$:, $O   |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, shocked(Dir)]);
html([$O, $:   |Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, shocked(Dir)]);
html([$:, $-, $O|Rest], Dir, SoFar)-> html(Rest, Dir, [SoFar, shocked(Dir)]);

%% =P   :P
html([$=, $P|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, mischief(Dir)]);
html([$:, $P|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, mischief(Dir)]);

%% :,(
html([$:, $,, $(|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, crying(Dir)]);

%% <3
html([$<, $3|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, heart(Dir)]);

html([229|Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&aring;"]);
html([228|Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&auml;"]);
html([246|Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&ouml;"]);
html([197|Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&Aring;"]);
html([196|Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&Auml;"]);
html([214|Rest], Dir, SoFar)  -> html(Rest, Dir, [SoFar, "&Ouml;"]);
html([$<|Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&lt;"]);
html([$>|Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&gt;"]);
html([$&|Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&amp;"]);
html([$"|Rest], Dir, SoFar)   -> html(Rest, Dir, [SoFar, "&quot;"]);
html([Char|Rest], Dir, SoFar) -> html(Rest, Dir, [SoFar, Char]).

heart(Dir)    -> smileHdr(Dir) ++ "heart.png' align=middle>&nbsp;".
happy(Dir)    -> smileHdr(Dir) ++ "happy.png' align=middle>&nbsp;".
sad(Dir)      -> smileHdr(Dir) ++ "sad.png' align=middle>&nbsp;".
wink(Dir)     -> smileHdr(Dir) ++ "wink.png' align=middle>&nbsp;".
lol(Dir)      -> smileHdr(Dir) ++ "lol.png' align=middle>&nbsp;".
shocked(Dir)  -> smileHdr(Dir) ++ "shocked.png' align=middle>&nbsp;".
crying(Dir)   -> smileHdr(Dir) ++ "crying.png' align=middle>&nbsp;".
mischief(Dir) -> smileHdr(Dir) ++ "mischief.png' align=middle>&nbsp;".

smileHdr(Dir) -> "&nbsp;<img class='emote' src='" ++ Dir ++ "/Icons/".

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
showStatus(User, Status, StatusMsg) ->
    OnLoad = "OnLoad=\"setTimeout('checkStatus()', 100);\" id='theBody'",
    [pageHeader(OnLoad, User),
     divTag([{id, "timerContainer"}],
            [divTag([{id, "timerUser"}],      [User]),
             divTag([{id, "timerStatus"}],    [Status]),
             divTag([{id, "timerStatusMsg"}], [makeHtml(StatusMsg)]),
             divTag([{id, "timerField"}],     [])]),
     pageFooter()].

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
    Opts = [{width, "20%"}, {align, center}],
    {EstimateSum, SpentSum, RemainingSum} = sum(Uids2),
    tableTag([makeTimeLogReport2(Uids2, []),
              trTag([{bgcolor, "black"}],
                    [tdTag([{width, "40%"}], heading("Total")),
                     tdTag(Opts, heading(EstimateSum)),
                     tdTag(Opts, heading(SpentSum)),
                     tdTag(Opts, heading(RemainingSum))
                    ])]).

makeTimeLogReport2([Uid|Rest], Acc) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    AllLogged = eTodoDB:getAllLoggedWork(Uid),
    {ok, _Desc, _ShowInWL, ShowInTL} = eTodoDB:getWorkDescAll(Uid),
    Todo = eTodoDB:getTodo(tryInt(Uid)),
    Done = Todo#todo.status == done,
    case {{Estimate, Remaining, AllLogged}, ShowInTL, Done} of
        {{0, 0, "00:00"}, false, _Done} ->
            makeTimeLogReport2(Rest, Acc);
        {_Time, false, true} ->
            makeTimeLogReport2(Rest, Acc);
        _ ->
            Desc1     = eTodoDB:getWorkDesc(Uid),
            Desc2     = eGuiFunctions:getWorkDesc(Desc1, Todo#todo.description),
            Odd       = ((length(Acc) rem 2) =/= 0),
            Opts2     = [{width, "20%"}, {align, center}],
            Opts3     = [{width, "20%"}],
            UidInt    = tryInt(Uid),
            UidStr    = eTodoUtils:convertUid(UidInt),
            Date      = date(),

            Link1  = aTag([{href, convertUid(UidInt, Date)}], time(Estimate) ++ ":00"),
            Link2  = aTag([{href, convertUid(UidInt, incDate(Date, 1))}], AllLogged),
            Link3  = aTag([{href, convertUid(UidInt, incDate(Date, 2))}], time(Remaining) ++ ":00"),

            makeTimeLogReport2(Rest,
                [trTag(bgColor(Odd),
                    [tdTag(Opts3, [aTag([{href, UidStr}], Desc2)]),
                        tdTag(Opts2, Link1),
                        tdTag(Opts2, Link2),
                        tdTag(Opts2, Link3)]) | Acc])
    end;
makeTimeLogReport2([], Result) ->
    Opts = [{width, "20%"}, {align, center}],
    [trTag([{bgcolor, "black"}],
           [tdTag([{width, "40%"}], heading("Task description")),
            tdTag(Opts, heading("Time estimate(h)")),
            tdTag(Opts, heading("Time spent(h)")),
            tdTag(Opts, heading("Time left(h)"))
           ])| lists:reverse(Result)].

%%======================================================================
%% Function : showTimeReport(User) -> Html
%% Purpose  : Make time html report.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
showTimeReport(User) ->
    {ok, Rows, AllTask} = eTodo:getTimeReportData(),
    showTimeReport(User, Rows, AllTask).

showTimeReport(_User, Rows, AllTask) ->
    Uids1 = [ETodo#etodo.uid || ETodo <- eRows:toList(Rows)],
    Uids2 = case AllTask of
                true ->
                    Uids1; %% Subtodos all ready in list.
                false ->
                    addSubTodos(Uids1)
            end,
    {EstimateSum, SpentSum, RemainingSum} = sum(Uids2),
    tableTag([showTimeReport2(Uids2, []),
            trTag([{class, "timeReportTable"}],
                [tdTag([{class, "timeReportDesc timeReportSum"}], "Total"),
                    tdTag([{class, "timeReportSum timeReportColumn"}], EstimateSum),
                    tdTag([{class, "timeReportSum timeReportColumn"}], SpentSum),
                    tdTag([{class, "timeReportSum timeReportColumn"}], RemainingSum)
                ])]).

showTimeReport2([Uid|Rest], Acc) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    AllLogged = eTodoDB:getAllLoggedWork(Uid),
    {ok, _Desc, _ShowInWL, ShowInTL} = eTodoDB:getWorkDescAll(Uid),
    Todo = eTodoDB:getTodo(tryInt(Uid)),
    Done = Todo#todo.status == done,
    case {{Estimate, Remaining, AllLogged}, ShowInTL, Done} of
        {{0, 0, "00:00"}, false, _Done} ->
            showTimeReport2(Rest, Acc);
        {_Time, false, true} ->
            showTimeReport2(Rest, Acc);
        _ ->
            Desc   = eTodoDB:getWorkDesc(Uid),
            Odd    = ((length(Acc) rem 2) =/= 0),
            Opts   = if Odd  -> [{class, "trOdd"}];
                        true -> [{class, "trEven"}]
                     end,
            Opts2  = [{class, "timeReportValue"}],
            UidInt = tryInt(Uid),
            UidStr = eTodoUtils:convertUid(UidInt),

            showTimeReport2(Rest,
                [trTag(Opts,
                    [tdTag(aTag([{href, "/eTodo/eWeb:showTodo?uid=" ++
                        http_uri:encode(UidStr)}], empty(Desc, Uid))),
                        tdTag(Opts2, time(Estimate) ++ ":00"),
                        tdTag(Opts2, AllLogged),
                        tdTag(Opts2, time(Remaining) ++ ":00")]) | Acc])
    end;
showTimeReport2([], Result) ->
    Opts = [{class, "timeReportColumn"}],
    [trTag([{class, "timeReportHeading"}],
        [tdTag([{class, "timeReportDesc"}], "Task description"),
            tdTag(Opts, "Time estimate(h)"),
            tdTag(Opts, "Time spent(h)"),
            tdTag(Opts, "Time left(h)")
        ])| lists:reverse(Result)].

sum(Uids) ->
    sum(Uids, {0, {0, 0}, 0}).

sum([], {Est, {Hours, Min}, Rem}) ->
    SumHours   = Hours + (Min div 60),
    SumMinutes = Min rem 60,
    {time(Est) ++ ":00",
     time(SumHours) ++ ":" ++ time(SumMinutes),
     time(Rem) ++ ":00"};
sum([Uid|Rest], {Est, {Hours, Min}, Rem}) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    {WHours, WMin}        = eTodoDB:getAllLoggedWorkInt(Uid),
    sum(Rest, {Est + Estimate, {WHours + Hours, WMin + Min}, Rem + Remaining}).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
addSubTodos(Uids) ->
    addSubTodos(Uids, []).

addSubTodos([], Acc) ->
    lists:reverse(Acc);
addSubTodos([Uid|Rest], Acc) ->
    SUids = addSubTodos(eTodoDB:getSubTodos(tryInt(Uid))),
    Acc2  = addToAccNoDuplicate([Uid|SUids], Acc),
    addSubTodos(Rest, Acc2).

addToAccNoDuplicate([], Acc) ->
    Acc;
addToAccNoDuplicate([Uid|Rest], Acc) ->
    addToAccNoDuplicate(Rest, [toStr(Uid)|lists:delete(Uid, Acc)]).

%%======================================================================
%% Function : makeSceduleReport(User) -> Html
%% Purpose  : Make scedule html report.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeSceduleReport(User) ->
    Opts        = [{width, "25%"}, {align, center}],
    Opts2       = [{width, "20%"}, {align, center}],
    Alarms      = getAlarmList(User),
    ETodos      = getTodoInfo(User),
    TimeMarkers = getTimeMarkers(),
    Events = lists:reverse(lists:keysort(1, TimeMarkers ++ Alarms ++ ETodos)),
    tableTag([trTag([{bgcolor, "black"}],
                    [tdTag([{width, "35%"}], heading("Description")),
                     tdTag(Opts,  heading("Next alarm")),
                     tdTag(Opts2, heading("Due date")),
                     tdTag(Opts2, heading("Time left(h)"))])|
              makeSceduleReport2(Events, [])]).

getTodoInfo(User) ->
    TodoList  = eTodoDB:getTodos(User, ?defTaskList),
    getTodoInfo(TodoList, date(), []).

getTodoInfo([], _Now, Acc) ->
    Acc;
getTodoInfo([#todo{status = done}|Rest], Now, Acc) ->
    getTodoInfo(Rest, Now, Acc);
getTodoInfo([#todo{dueTime = undefined}|Rest], Now, Acc) ->
    getTodoInfo(Rest, Now, Acc);
getTodoInfo([#todo{uid = Uid, dueTime = DueDate}|Rest], Now, Acc) ->
    NextAlarm = {DueDate, {0, 0, 0}},
    {Desc, DueDate, UidStr, RemTime} = doSaveAlarmInfo(Uid),
    getTodoInfo(Rest, Now, [{NextAlarm, "", DueDate,
                             UidStr, Desc, RemTime} | Acc]);
getTodoInfo([_Todo|Rest], Now, Acc) ->
    getTodoInfo(Rest, Now, Acc).

getAlarmList(User) ->
    AlarmList  = eTodoDB:getReminders(User),
    getAlarmInfo(AlarmList, date(), []).

getAlarmInfo([], _Now, Acc) ->
    Acc;
%% New alarm
getAlarmInfo([#alarmCfg{startDate  = StartDate,
                        startTime  = StartTime,
                        uid        = Uid,
                        nextAlarm  = undefined}|Rest], Now, Acc)
  when Now =< StartDate ->
    NextAlarm = {StartDate, repairTime(StartTime)},
    saveAlarmInfo(Uid, Rest, Now, NextAlarm, Acc);
%% Alarm that never will be activated
getAlarmInfo([#alarmCfg{nextAlarm = never}|Rest], Now, Acc) ->
    getAlarmInfo(Rest, Now, Acc);
getAlarmInfo([#alarmCfg{uid = Uid, nextAlarm  = {Date, Time}}|Rest], Now, Acc)
        when Now =< Date ->
    saveAlarmInfo(Uid, Rest, Now, {Date, Time}, Acc);
getAlarmInfo([_Alarm|Rest], Now, Acc) ->
    getAlarmInfo(Rest, Now, Acc).

saveAlarmInfo(Uid, Rest, Now, NextAlarm, Acc) ->
    {Desc, DueDate, UidStr, RemTime} = doSaveAlarmInfo(Uid),
    getAlarmInfo(Rest, Now, [{NextAlarm, NextAlarm, DueDate,
                              UidStr, Desc, RemTime} | Acc]).

doSaveAlarmInfo(Uid) ->
    Todo    = eTodoDB:getTodo(tryInt(Uid)),
    Desc1   = eTodoDB:getWorkDesc(toStr(Uid)),
    Desc2   = eGuiFunctions:getWorkDesc(Desc1, Todo#todo.description),
    DueDate = Todo#todo.dueTime,
    UidStr  = eTodoUtils:convertUid(tryInt(Uid)),
    {_, RemTime} = eTodoDB:getTime(toStr(Uid)),
    {Desc2, DueDate, UidStr, RemTime}.

makeSceduleReport2([], Acc) ->
    Acc;
makeSceduleReport2([{_Key, Desc}|Rest], Acc) ->
    Odd = ((length(Rest) rem 2) =/= 0),
    makeSceduleReport2(Rest, [trTag(bgColor(Odd),
                                    [tdTag([{width, "35%"}], bTag(Desc)),
                                     tdTag([{width, "25%"}], ""),
                                     tdTag([{width, "20%"}], ""),
                                     tdTag([{width, "20%"}], "")])|Acc]);
makeSceduleReport2([{_Key, DateTime, DueDate,
                     UidStr, Desc, RemTime}|Rest], Acc) ->
    Odd = ((length(Rest) rem 2) =/= 0),
    makeSceduleReport2(Rest, [trTag(bgColor(Odd),
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
                                           toStr(RemTime))])|Acc]).

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

repairTime({H, M})    -> {H, M, 0};
repairTime(undefined) -> {0, 0, 0};
repairTime(Time)      -> Time.