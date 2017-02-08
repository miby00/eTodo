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
         eMenuEvent/6]).

getName() -> "JIRA".

getDesc() -> "Create eTodo task from JIRA issue and report work logs.".

-record(state, {frame, jiraUrl, jiraSearch, bauth}).

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
    #state{frame = Frame, jiraUrl = Url, jiraSearch = SearchCfg, bauth = BAuth}.

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
getMenu(undefined,
        State = #state{jiraUrl = JiraUrl, jiraSearch = Search, bauth = BAuth}) ->
    Url = JiraUrl ++ "/rest/api/2/search?jql=" ++
                     http_uri:encode(Search) ++ "&fields=summary,key",
    Result  = httpRequest(BAuth, get, Url),
    SubMenu = constructSubMenu(1501, Result),
    {ok, [{{subMenu, "Create Task from JIRA"}, SubMenu}], State};
getMenu(ETodo,
        State = #state{jiraUrl = JiraUrl, jiraSearch = Search, bauth = BAuth}) ->
    Url = JiraUrl ++ "/rest/api/2/search?jql=" ++
        http_uri:encode(Search) ++ "&fields=summary,key",
    Result  = httpRequest(BAuth, get, Url),
    SubMenu = constructSubMenu(1501, Result),
    Comment = lists:flatten(ETodo#etodo.comment),
    case parseComment(Comment, JiraUrl) of
        {error, keyNotFound} ->
            {ok, [{{subMenu, "Create Task from JIRA"},
                  SubMenu}], State};
        _ ->
            {ok, [{1500, "Log work in JIRA"},
                  {{subMenu, "Create Task from JIRA"},
                   SubMenu}], State}
    end.

constructSubMenu(MenuOption, Result) ->
    MapResult = jsx:decode(Result, [return_maps]),
    Issues    = maps:get(<<"issues">>, MapResult),
    constructSubMenu2(MenuOption, Issues, []).

constructSubMenu2(_MenuOption, [], SoFar) ->
    SoFar;
constructSubMenu2(MenuOption, [Feature|Rest], SoFar) ->
    FeatureRef   = binary_to_list(maps:get(<<"key">>, Feature)),
    Fields       = maps:get(<<"fields">>, Feature),
    FeatureDesc  = unicode:characters_to_binary(maps:get(<<"summary">>, Fields),
                                                utf8, latin1),
    FeatureMText = FeatureRef ++ ": " ++ binary_to_list(FeatureDesc),
    constructSubMenu2(MenuOption + 1, Rest, [{MenuOption, FeatureMText}|SoFar]).

%%--------------------------------------------------------------------
%% @doc
%% Called every 15 seconds to check if someone changes status
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
            WorkLogs2   = [filterWorkLogs(WorkLog) || WorkLog <- WorkLogs],
            LoggedWork  = eTodoDB:getAllLoggedWorkDate(ETodo#etodo.uid),
            LoggedWork2 = calcWorkToLog(LoggedWork, WorkLogs2),

            {_Est,     Remaining} = eTodoDB:getTime(ETodo#etodo.uid),
            {Choices, Selections} = constructMessage(LoggedWork2),

            MultiDlg   = wxMultiChoiceDialog:new(Frame,
                                                 "Choose which updates to apply",
                                                 "Update work log", Choices),
            wxMultiChoiceDialog:setSize(MultiDlg, {300, 300}),
            wxMultiChoiceDialog:setSelections(MultiDlg, Selections),
            case wxMultiChoiceDialog:showModal(MultiDlg) of
                ?wxID_OK ->
                    MSel   = wxMultiChoiceDialog:getSelections(MultiDlg),
                    TTLog  = [lists:nth(I, LoggedWork2) || I <- MSel, I =/= 0],
                    SetRem = {lists:member(0, MSel), Remaining},
                    logTime(TTLog, BAuth, FullUrl, SetRem);
                ?wxID_CANCEL ->
                    ok
            end,
            wxMultiChoiceDialog:destroy(MultiDlg),
            State
    end;
eMenuEvent(_EScriptDir, User, _MenuOption, _ETodo, MenuText,
           State = #state{jiraUrl = JiraUrl, bauth = BAuth}) ->
    [Key|_]   = string:tokens(MenuText, ":"),
    BaseUrl   = JiraUrl ++ "/rest/api",
    FullUrl   = BaseUrl ++ "/2/issue/" ++ Key ++ "/?fields=description,status,summary,priority",
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
    Priority  = get(<<"id">>, get(<<"priority">>, Fields, <<>>), 2),
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

    Todo = #todo{uid         = eTodoUtils:makeRef(),
                 status      = status2DB(Status),
                 priority    = prio2DB(Priority),
                 description = binary_to_list(Desc3),
                 comment     = makeComment([IssUrl|Comments2]),
                 progress    = 0,
                 createTime  = eTodoUtils:dateTime()},

    TaskList = eTodo:getTaskList(),
    Row = eTodoDB:getRow(User, TaskList),
    eTodoDB:addTodo(#userInfo{userName = User,
                              uid      = Todo#todo.uid,
                              row      = Row,
                              parent   = eTodoUtils:tryInt(TaskList)}, Todo),

    eTodo:todoCreated(TaskList, Row, Todo),
    State.

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
characters_to_binary(Value) ->
    unicode:characters_to_binary(Value, utf8, latin1).

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

filterWorkLogs(#{<<"author">>           := #{<<"name">>         := Name,
                                             <<"emailAddress">> := Email,
                                             <<"active">>       := Active},
                 <<"comment">>          := Comment,
                 <<"started">>          := LogTime,
                 <<"id">>               := Id,
                 <<"timeSpentSeconds">> := TimeSpent}) ->
    <<LogDate:10/bytes, _/binary>> = LogTime,
    {LogDate, #{name             => Name,
                email            => Email,
                active           => Active,
                comment          => Comment,
                workLogId        => Id,
                timeSpentSeconds => TimeSpent}};
filterWorkLogs(_WorkLog) ->
    {undefined, #{}}.

calcWorkToLog(LoggedWork, WorkLogs) ->
    calcWorkToLog(LoggedWork, WorkLogs, []).

calcWorkToLog([], _, Acc) ->
    Acc;
calcWorkToLog([Work|Rest], WorkLogs, Acc) ->
    [Date, Hours, Minutes] = string:tokens(Work, " :"),
    Seconds = seconds(Hours, Minutes),
    BinDate = list_to_binary(Date),
    case lists:keyfind(BinDate, 1, WorkLogs) of
        {BinDate, WL = #{timeSpentSeconds := TimeSpent,
                      active           := true}} ->
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

httpPost(BAuth, Method, Body, Url) ->
    AuthOption  = {"Authorization", BAuth},
    ContentType = {"Content-Type", "application/json"},
    Request     = {Url, [AuthOption, ContentType], "application/json", Body},
    Options     = [{body_format,binary}],
    case httpc:request(Method, Request, [{url_encode, false}], Options) of
        {ok, {_StatusLine, _Headers, RBody}} ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, RBody],
                "http success", ?LINE),
            Body;
        Else ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, Else],
                "http failure", ?LINE),
            Else
    end.

constructMessage(LoggedWork) ->
    constructMessage(LoggedWork, {[], []}, 1).

constructMessage([], {Acc1, Acc2}, _Index) ->
    {["Set remaining to time left"|lists:reverse(Acc1)], Acc2};
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

logTime([], _BAuth, _FullUrl, _SetRemaining) ->
    ok;
logTime([{new, Date, Seconds}|Rest], BAuth, FullUrl, SetRemaining) ->
    Started  = iso8601(Date),
    JSON     = jsx:encode(#{<<"timeSpentSeconds">> => Seconds,
                            <<"comment">>          => <<"Logged from eTodo">>,
                            <<"started">>          => Started}),
    FullUrl2 = setRemaining(FullUrl, SetRemaining),
    httpPost(BAuth, post, JSON, FullUrl2),
    logTime(Rest, BAuth, FullUrl, SetRemaining);
logTime([{{_, WL}, _Date, Seconds}|Rest], BAuth, FullUrl, SetRemaining) ->
    JSON = jsx:encode(#{<<"timeSpentSeconds">> => Seconds,
                        <<"comment">>          => <<"Logged from eTodo">>}),
    FullUrl2 = FullUrl ++ "/" ++ binary_to_list(maps:get(workLogId, WL, <<>>)),
    FullUrl3 = setRemaining(FullUrl2, SetRemaining),
    httpPost(BAuth, put, JSON, FullUrl3),
    logTime(Rest, BAuth, FullUrl, SetRemaining).

setRemaining(FullUrl, {false, _}) ->
    FullUrl;
setRemaining(FullUrl, {true, Remaining}) ->
    FullUrl ++ "?adjustEstimate=new&newEstimate=" ++
               integer_to_list(Remaining) ++ "h".

iso8601(Date) ->
    {_, {Hour, Minute, Second}} = calendar:universal_time(),
    FmtStr = "T~2..0w:~2..0w:~2..0w.000+0000",
    iolist_to_binary(io_lib:format(Date ++ FmtStr, [Hour, Minute, Second])).