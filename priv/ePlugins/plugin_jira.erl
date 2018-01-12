%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_jira).

-export([getName/0, getDesc/0, getMenu/2, init/1, terminate/2]).

-export([eGetStatusUpdate/5,
         eTimerStarted/7,
         eTimerStopped/3,
         eTimerEnded/4,
         eReceivedMsg/5,
         eReceivedSysMsg/3,
         eReceivedAlarmMsg/3,
         eLoggedInMsg/3,
         eLoggedOutMsg/3,
         eSetStatusUpdate/5,
         eSetWorkLogDate/4,
         eMenuEvent/6]).

getName() -> "JIRA".

getDesc() -> "Create eTodo task from JIRA issue and report work logs.".

-record(state, {frame, jiraUser, jiraUrl, jiraSearch, bauth}).

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("wx/include/wx.hrl").

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init([WX, Frame]) ->
    {ok, Url}       = application:get_env(eTodo, jiraUrl),
    {ok, SearchCfg} = application:get_env(eTodo, jiraSearch),
    {ok, User}      = application:get_env(eTodo, jiraUser),
    {ok, Pwd}       = application:get_env(eTodo, jiraPwd),

    UserPwd         = User ++ ":" ++ Pwd,
    BAuth           = "Basic " ++ base64:encode_to_string(UserPwd),

    wx:set_env(WX),
    #state{frame      = Frame,
           jiraUser   = list_to_binary(User),
           jiraUrl    = Url,
           jiraSearch = SearchCfg,
           bauth      = BAuth}.

%%--------------------------------------------------------------------
%% @doc
%% Free internal data for plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Return key value list of right menu options.
%% Menu option should be a unique integer bigger than 1300.
%% @spec getMenu(ETodo, State) -> {ok, [{menuOption, menuText}, ...], NewState}
%% @end
%%--------------------------------------------------------------------
getMenu(undefined, State) ->
    {ok, [{1501, "Create Task"},
          {1502, "Create Task(s)"}], State};
getMenu(ETodo, State = #state{jiraUrl = JiraUrl}) ->
    Comment = lists:flatten(ETodo#etodo.comment),
    case parseComment(Comment, JiraUrl) of
        {error, keyNotFound} ->
            {ok, [{1501, "Create Task"},
                  {1502, "Create Task(s)"}], State};
        _ ->
            {ok, [{1500, "Log work in JIRA"},
                  {1501, "Create Task"},
                  {1502, "Create Task(s)"}], State}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Called every 5 seconds to check if someone changes status
%% outside eTodo.
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       {ok, Status, StatusMsg, NewState}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(_Dir, _User, Status, StatusMsg, State) ->
    {ok, Status, StatusMsg, State}.

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(EScriptDir, User, Text, Hours, Min, Sec, State) ->
%%                     NewState
%% @end
%%--------------------------------------------------------------------
eTimerStarted(_EScriptDir, _User, _Text, _Hours, _Min, _Sec, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(EScriptDir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerStopped(_EScriptDir, _User, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerEnded(_EScriptDir, _User, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(_EScriptDir, _User, _Users, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(_Dir, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(_Dir, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs in.
%%
%% @spec eLoggedInMsg(Dir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(_Dir, _User, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs out.
%%
%% @spec eLoggedOutMsg(Dir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(_Dir, _User, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(_Dir, _User, _Status, _StatusMsg, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes work log date
%% @spec eSetWorkLogDate(Dir, User, Date, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eSetWorkLogDate(_Dir, _User, _Date, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called for right click menu
%%
%% @spec eMenuEvent(EScriptDir, User, MenuOption, ETodo, MenuText, State) ->
%%                  NewState
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, _User, 1500, ETodo, _MenuText,
           State = #state{jiraUrl = JiraUrl, bauth = BAuth, frame = Frame}) ->
    Comment = lists:flatten(ETodo#etodo.comment),
    case parseComment(Comment, JiraUrl) of
        {error, keyNotFound} ->
            State;
        Key ->
            BaseUrl     = JiraUrl ++ "/rest/api",
            FullUrl     = BaseUrl ++ "/2/issue/" ++ Key ++ "/worklog",
            Result      = httpRequest(BAuth, get, FullUrl),
            MapRes      = jsx:decode(Result, [return_maps]),
            WorkLogs    = maps:get(<<"worklogs">>, MapRes, []),
            WorkLogs2   = filterWorkLogs(WorkLogs, State#state.jiraUser),
            LoggedWork  = ePluginInterface:getAllLoggedWorkDate(ETodo#etodo.uid),
            LoggedWork2 = calcWorkToLog(LoggedWork, WorkLogs2),

            {Estimate, Remaining} = ePluginInterface:getTime(ETodo#etodo.uid),
            {Choices, Selections} = constructMessage(LoggedWork2),

            MultiDlg   = wxMultiChoiceDialog:new(Frame,
                                                 "Choose which updates to apply",
                                                 "Update work log", Choices),
            wxMultiChoiceDialog:setSize(MultiDlg, {300, 300}),
            wxMultiChoiceDialog:setSelections(MultiDlg, Selections),
            case wxMultiChoiceDialog:showModal(MultiDlg) of
                ?wxID_OK ->
                    MSel   = wxMultiChoiceDialog:getSelections(MultiDlg),
                    TTLog  = [lists:nth(I - 1, LoggedWork2) || I <- MSel, I > 1],
                    logTime(TTLog, BAuth, FullUrl, Frame),
                    SetRem = {lists:member(0, MSel), Remaining},
                    SetEst = {lists:member(1, MSel), Estimate},
                    doSetRemaining(SetRem, BAuth,
                                   BaseUrl ++ "/2/issue/" ++ Key, Frame),
                    doSetEstimate(SetEst,  BAuth,
                                  BaseUrl ++ "/2/issue/" ++ Key, Frame);
                ?wxID_CANCEL ->
                    ok
            end,
            wxMultiChoiceDialog:destroy(MultiDlg),
            State
    end;
eMenuEvent(_EScriptDir, User, 1501, _ETodo, _MenuText,
           State = #state{frame = Frame}) ->
    EntryDlg = wxTextEntryDialog:new(Frame,
                                     "Enter issue to create task from"),
    wxTextEntryDialog:setValue(EntryDlg, "DEV-"),
    case wxTextEntryDialog:showModal(EntryDlg) of
        ?wxID_OK ->
            Value = wxTextEntryDialog:getValue(EntryDlg),
            case verifyValue(Value) of
                true ->
                    addTask(User, Value, State);
                false ->
                    ok
            end;
        ?wxID_CANCEL ->
            ok
    end,
    wxTextEntryDialog:destroy(EntryDlg),
    State;
eMenuEvent(_EScriptDir, User, 1502, _ETodo, _MenuText,
           State = #state{jiraUrl = JiraUrl, jiraSearch = Search,
                          bauth   = BAuth,   frame      = Frame}) ->
    Url = JiraUrl ++ "/rest/api/2/search?jql=" ++
        http_uri:encode(Search) ++ "&maxResults=5000&fields=summary,key",
    Result   = httpRequest(BAuth, get, Url),
    Issues   = issueList(Result),
    MultiDlg = wxMultiChoiceDialog:new(Frame,
                                       "Choose which issues to create tasks from",
                                       "Create task", Issues),
    wxMultiChoiceDialog:setSize(MultiDlg, {500, 500}),
    case wxMultiChoiceDialog:showModal(MultiDlg) of
        ?wxID_OK ->
            MSel = wxMultiChoiceDialog:getSelections(MultiDlg),
            [addTask(User, lists:nth(I + 1, Issues), State) || I <- MSel];
        ?wxID_CANCEL ->
            ok
    end,
    wxMultiChoiceDialog:destroy(MultiDlg),
    State.

addTask(User, MenuText, State= #state{jiraUrl = JiraUrl, bauth = BAuth}) ->
    [Key|_]   = string:tokens(MenuText, ":"),
    BaseUrl   = JiraUrl ++ "/rest/api",
    FullUrl   = BaseUrl ++ "/2/issue/" ++ Key ++
        "/?fields=description,status,summary,priority,timetracking",
    Result    = httpRequest(BAuth, get, FullUrl),
    MapRes    = jsx:decode(Result, [return_maps]),
    Fields    = get(<<"fields">>, MapRes, #{}),
    Summary   = get(<<"summary">>, Fields, <<>>),
    Desc      = get(<<"description">>, Fields, Summary),
    Desc3     = case Desc of
                    Summary ->
                        characters_to_binary(Desc);
                    Desc ->
                        Desc2 = <<Summary/binary, "\n\n", Desc/binary>>,
                        characters_to_binary(Desc2)
                end,
    Status    = get(<<"name">>, get(<<"status">>, Fields, #{}), <<>>),
    Priority  = get(<<"id">>, get(<<"priority">>, Fields, <<>>), <<"2">>),
    TimeTrac  = get(<<"timetracking">>, Fields, #{}),
    Estimate  = get(<<"originalEstimateSeconds">>, TimeTrac, 0),
    Remaining = get(<<"remainingEstimateSeconds">>, TimeTrac, 0),
    ComUrl    = BaseUrl ++ "/2/issue/" ++ Key ++ "/comment",
    Result2   = httpRequest(BAuth, get, ComUrl),
    MapRes2   = jsx:decode(Result2, [return_maps]),
    Comments1 = get(<<"comments">>, MapRes2, []),
    Comments2 = [{get(<<"created">>, Comment, <<>>),
                  get(<<"body">>,    Comment, <<>>),
                  get(<<"displayName">>,
                      get(<<"author">>, Comment, #{}), <<>>)}
                 || Comment <- Comments1],
    IssUrl     = "<" ++ JiraUrl ++ "/browse/" ++ Key ++ ">",

    Todo = #todo{uid         = ePluginInterface:makeRef(),
                 status      = status2DB(Status),
                 priority    = prio2DB(Priority),
                 description = binary_to_list(Desc3),
                 comment     = makeComment([IssUrl|Comments2]),
                 progress    = 0,
                 createTime  = ePluginInterface:dateTime()},

    TaskList = ePluginInterface:getTaskList(),
    Row    = ePluginInterface:getRow(User, TaskList),
    Parent = ePluginInterface:tryInt(TaskList),
    ePluginInterface:addTodo(#userInfo{userName = User,
                                       uid      = Todo#todo.uid,
                                       row      = Row,
                                       parent   = Parent}, Todo),

    ePluginInterface:todoCreated(TaskList, Row, Todo),

    ePluginInterface:saveTime(Todo#todo.uid, Estimate, Remaining),
    State.

issueList(Result) ->
    MapResult = jsx:decode(Result, [return_maps]),
    Issues    = maps:get(<<"issues">>, MapResult),
    issueList2(Issues, []).

issueList2([], SoFar) ->
    SoFar;
issueList2([Feature| Rest], SoFar) ->
    FeatureRef   = binary_to_list(maps:get(<<"key">>, Feature)),
    Fields       = maps:get(<<"fields">>, Feature),
    FeatureDesc  = toLatin1(maps:get(<<"summary">>, Fields)),
    FeatureMText = FeatureRef ++ ": " ++ binary_to_list(FeatureDesc),
    issueList2(Rest, [FeatureMText| SoFar]).

status2DB(<<"In Progress">>) -> inProgress;
status2DB(_)                 -> planning.

prio2DB(<<"1">>) -> low;
prio2DB(<<"2">>) -> medium;
prio2DB(<<"3">>) -> high;
prio2DB(_Prio)   -> high.

makeComment(Comments) ->
    makeComment(Comments, []).

makeComment([], Acc) ->
    lists:reverse(Acc);
makeComment([{Created, Body, Author}|Rest], Acc)
  when is_binary(Created), is_binary(Body), is_binary(Author) ->
    Created2 = convertDate(Created),
    Msg  = <<Created2/binary, " ", Author/binary, ": ", Body/binary, "\n\n">>,
    Msg2 = characters_to_binary(Msg),
    Acc2 = [binary_to_list(Msg2)|Acc],
    makeComment(Rest, Acc2);
makeComment([Value|Rest], Acc) when is_list(Value) ->
    makeComment(Rest, [Value ++ "\n\n"|Acc]);
makeComment([_Value|Rest], Acc) ->
    makeComment(Rest, Acc).

characters_to_binary(null)  -> <<>>;
characters_to_binary(Value) -> toLatin1(Value).

get(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        null ->
            Default;
        Value ->
            Value
    end.

convertDate(<<Date:10/bytes, _/binary>>) ->
    Date;
convertDate(Value) ->
    Value.

parseComment(Comment, BaseUrl) ->
    case string:str(Comment, BaseUrl) of
        0 ->
            {error, keyNotFound};
        Pos ->
            Rest = string:substr(Comment, Pos + length(BaseUrl)),
            findKey(Rest)
    end.

findKey("/browse/" ++ Rest) ->
    findKey(Rest, []);
findKey(_Rest) ->
    {error, keyNotFound}.

findKey([], _Acc) ->
    {error, keyNotFound};
findKey([$>|_], Acc) ->
    lists:reverse(Acc);
findKey([Char|Rest], Acc) ->
    findKey(Rest, [Char|Acc]).

filterWorkLogs(WorkLogs, JiraUser) when is_list(WorkLogs) ->
    filterWorkLogs(WorkLogs, JiraUser, []).

filterWorkLogs([], _JiraUser, Acc) ->
    lists:reverse(Acc);
filterWorkLogs([#{<<"author">>           := #{<<"name">>        := Name,
                                              <<"emailAddress">> := Email},
                  <<"comment">>          := Comment,
                  <<"started">>          := LogTime,
                  <<"id">>               := Id,
                  <<"timeSpentSeconds">> := TimeSpent}|Rest], JiraUser, Acc)
  when (Name == JiraUser) ->
    <<LogDate:10/bytes, _/binary>> = LogTime,
    Acc2 = [{LogDate, #{name             => Name,
                        email            => Email,
                        comment          => Comment,
                        workLogId        => Id,
                        timeSpentSeconds => TimeSpent}}|Acc],
    filterWorkLogs(Rest, JiraUser, Acc2);
filterWorkLogs([_WL|Rest], JiraUser, Acc) ->
    filterWorkLogs(Rest, JiraUser, Acc).

calcWorkToLog(LoggedWork, WorkLogs) ->
    calcWorkToLog(LoggedWork, WorkLogs, []).

calcWorkToLog([], _, Acc) ->
    Acc;
calcWorkToLog([Work|Rest], WorkLogs, Acc) ->
    [Date, Hours, Minutes] = string:tokens(Work, " :"),
    Seconds = seconds(Hours, Minutes),
    BinDate = list_to_binary(Date),
    case lists:keyfind(BinDate, 1, WorkLogs) of
        {BinDate, WL = #{timeSpentSeconds := TimeSpent}} ->
            case TimeSpent >= Seconds of
                true ->
                    calcWorkToLog(Rest, WorkLogs,
                                  [{{done, WL}, Date, Seconds}|Acc]);
                false ->
                    calcWorkToLog(Rest, WorkLogs,
                                  [{{update, WL}, Date, Seconds}|Acc])
            end;
        _ ->
            calcWorkToLog(Rest, WorkLogs, [{new, Date, Seconds}|Acc])
    end.

seconds(Hours, Minutes) ->
    3600 * list_to_integer(Hours) + 60 * list_to_integer(Minutes).

httpRequest(BAuth, Method, Url) ->
    AuthOption  = {"Authorization", BAuth},
    ContentType = {"Content-Type", "application/json"},
    Request     = {Url, [AuthOption, ContentType]},
    Options     = [{body_format,binary}],
    case httpc:request(Method, Request, [{url_encode, false}], Options) of
        {ok, {_StatusLine, _Headers, Body}} ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body],
                     "http success", ?LINE),
            Body;
        Else ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Else],
                     "http failure", ?LINE),
            Else
    end.

httpPost(BAuth, Method, Body, Url, Frame) ->
    AuthOption  = {"Authorization", BAuth},
    ContentType = {"Content-Type", "application/json"},
    Request     = {Url, [AuthOption, ContentType], "application/json", Body},
    Options     = [{body_format,binary}],
    case httpc:request(Method, Request, [{url_encode, false}], Options) of
        {ok, {{_HTTPVersion, Status, _Reason}, _Headers, RBody}}
          when (Status == 200) or (Status == 201) or (Status == 204) ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, RBody],
                     "http success", ?LINE),
            Body;
        {ok, {{_HTTPVersion, _Error, Reason}, _Headers, RBody}} ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, RBody],
                     "http failure", ?LINE),
            MsgDlg = wxMessageDialog:new(Frame, "Error occured: " ++ Reason,
                                         [{style,   ?wxICON_ERROR},
                                          {caption, "Worklog failure"}]),
            wxMessageDialog:showModal(MsgDlg),
            wxMessageDialog:destroy(MsgDlg),
            Body;
        Else ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, Else],
                     "http failure", ?LINE),
            Else
    end.

constructMessage(LoggedWork) ->
    constructMessage(LoggedWork, {[], []}, 2).

constructMessage([], {Acc1, Acc2}, _Index) ->
    {["Set remaining to time left",
      "Set estimate to time estimate"|lists:reverse(Acc1)], Acc2};
constructMessage([{new, Date, Seconds}|Rest], {Acc1, Acc2}, Index) ->
    NAcc1 = [Date ++ " Time spent: " ++
                 binary_to_list(convertSeconds2Jira(Seconds))|Acc1],
    constructMessage(Rest, {NAcc1, [Index|Acc2]}, Index + 1);
constructMessage([{{update, _}, Date, Seconds}|Rest], {Acc1, Acc2}, Index) ->
    NAcc1 = [Date ++ " Time spent: " ++
                 binary_to_list(convertSeconds2Jira(Seconds))|Acc1],
    constructMessage(Rest, {NAcc1, [Index|Acc2]}, Index + 1);
constructMessage([{_, Date, Seconds}|Rest], {Acc1, Acc2}, Index) ->
    NAcc1 = [Date ++ " Time spent: " ++
                 binary_to_list(convertSeconds2Jira(Seconds))|Acc1],
    constructMessage(Rest, {NAcc1, Acc2}, Index + 1).


convertSeconds2Jira(Seconds) ->
    Hours = Seconds div 3600,
    Min   = (Seconds - Hours * 3600) div 60,
    constructJiraTime(Hours, Min).

constructJiraTime(0, 0) ->
    <<>>;
constructJiraTime(0, Min) ->
    list_to_binary(integer_to_list(Min) ++ "m");
constructJiraTime(Hours, 0) ->
    list_to_binary(integer_to_list(Hours) ++ "h");
constructJiraTime(Hours, Min) ->
    HBin = list_to_binary(integer_to_list(Hours) ++ "h"),
    MBin = list_to_binary(integer_to_list(Min) ++ "m"),
    <<HBin/binary, MBin/binary>>.

logTime([], _BAuth, _FullUrl, _Frame) ->
    ok;
logTime([{new, Date, Seconds}|Rest], BAuth, FullUrl, Frame) ->
    Started  = iso8601(Date),
    JSON     = jsx:encode(#{<<"timeSpentSeconds">> => Seconds,
                            <<"comment">>          => <<"Logged from eTodo">>,
                            <<"started">>          => Started}),
    httpPost(BAuth, post, JSON, FullUrl, Frame),
    logTime(Rest, BAuth, FullUrl, Frame);
logTime([{{_, WL}, _Date, Seconds}|Rest], BAuth, FullUrl, Frame) ->
    JSON = jsx:encode(#{<<"timeSpentSeconds">> => Seconds,
                        <<"comment">>          => <<"Logged from eTodo">>}),
    FullUrl2 = FullUrl ++ "/" ++ binary_to_list(maps:get(workLogId, WL, <<>>)),
    httpPost(BAuth, put, JSON, FullUrl2, Frame),
    logTime(Rest, BAuth, FullUrl, Frame).

iso8601(Date) ->
    {_, {Hour, Minute, Second}} = calendar:universal_time(),
    FmtStr = "T~2..0w:~2..0w:~2..0w.000+0000",
    iolist_to_binary(io_lib:format(Date ++ FmtStr, [Hour, Minute, Second])).

doSetRemaining({true, Remaining}, BAuth, Url, Frame) ->
    Jrem = list_to_binary(integer_to_list(Remaining) ++ "h"),
    Edit = #{<<"edit">> => #{<<"remainingEstimate">> => Jrem}},
    JSON = jsx:encode(#{<<"update">> => #{<<"timetracking">> => [Edit]}}),
    httpPost(BAuth, put, JSON, Url, Frame);
doSetRemaining(_SetRemaining, _BAuth, _Url, _Frame) ->
    ok.

doSetEstimate({true, Estimate}, BAuth, Url, Frame) ->
    Jrem = list_to_binary(integer_to_list(Estimate) ++ "h"),
    Edit = #{<<"edit">> => #{<<"originalEstimate">> => Jrem}},
    JSON = jsx:encode(#{<<"update">> => #{<<"timetracking">> => [Edit]}}),
    httpPost(BAuth, put, JSON, Url, Frame);
doSetEstimate(_SetEstimate, _BAuth, _Url, _Frame) ->
    ok.

verifyValue("DEV-" ++ Rest) ->
    case catch list_to_integer(Rest) of
        Num when is_integer(Num), Num > 0 ->
            true;
        _ ->
            false
    end.

toLatin1(Bin) ->
  toLatin1(Bin, <<>>).

toLatin1(Bin, Acc) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, latin1) of
        Binary when is_binary(Binary) ->
            <<Acc/binary, Binary/binary>>;
        {error, Binary, Rest} ->
            BinRest = removeUtf8Char(Rest),
            toLatin1(BinRest, <<Acc/binary, Binary/binary>>);
        {incomplete, Binary, Rest} ->
            BinRest = removeUtf8Char(Rest),
            toLatin1(BinRest, <<Acc/binary, Binary/binary>>)
    end.

removeUtf8Char(<<2#0:1,    _Y:7, T/binary>>) -> T;
removeUtf8Char(<<2#110:3,  _Y:13,T/binary>>) -> T;
removeUtf8Char(<<2#1110:4, _Y:20,T/binary>>) -> T;
removeUtf8Char(<<2#11110:5,_Y:27,T/binary>>) -> T;
removeUtf8Char(<<_:8, T/binary>>)            -> T;
removeUtf8Char(_)                            -> <<>>.
