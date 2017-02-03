%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_jira).

-export([getName/0, getDesc/0, getMenu/2, init/0, terminate/2]).

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

getDesc() -> "Create JIRA task from eTodo.".

-record(state, {config = defaultConfig()}).

-include_lib("eTodo/include/eTodo.hrl").

-define(configFile, "jira.config").

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init() ->
    Config = case file:consult(?configFile) of
                 {ok, [Cfg]} when is_map(Cfg) ->
                     Cfg;
                 _ ->
                     defaultConfig()
             end,
    UserPwd     = maps:get(user, Config) ++ ":" ++ maps:get(pwd, Config),
    BAuth       = "Basic " ++ base64:encode_to_string(UserPwd),
    Config2     = Config#{bauth := BAuth},
    case filelib:is_file(?configFile) of
        true ->
            ok;
        false ->
            file:write_file(?configFile, io_lib:format("~tp.~n", [Config2]))
    end,
    #state{config = Config2}.

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
getMenu(_ETodo, State = #state{config = Config}) ->
    #{search   := Search,
      baseurl  := BaseUrl} = Config,
    Url         = BaseUrl ++ "/rest/api/2/search?jql=" ++
        http_uri:encode(Search) ++ "&fields=summary,key",
    Result      = httpRequest(Config, get, Url),
    SubMenu     = constructSubMenu(1502, Result),
    {ok, [{1500, "Log work in JIRA"},
          {1501, "Show logged work in JIRA"},
          {{subMenu, "Create Task from JIRA"},
           SubMenu}], State}.

httpRequest(#{bauth := BAuth}, Method, Url) ->
    AuthOption  = {"Authorization", BAuth},
    ContentType = {"Content-Type", "application/json"},
    Request     = {Url, [AuthOption, ContentType]},
    Options     = [{body_format,binary}],
    case httpc:request(Method, Request, [{url_encode, false}], Options) of
        {ok, {_StatusLine, _Headers, Body}} ->
            file:write_file("json.txt", Body),
            Body;
        Else ->
            Else
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
eMenuEvent(_EScriptDir, _User, 1500, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    State;
eMenuEvent(_EScriptDir, _User, 1501, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    State;
eMenuEvent(_EScriptDir, User, _MenuOption, _ETodo, MenuText,
           State = #state{config = Config}) ->
    [Key|_]   = string:tokens(MenuText, ":"),
    BaseUrl   = maps:get(baseurl, Config) ++ "/rest/api",
    FullUrl   = BaseUrl ++ "/2/issue/" ++ Key ++ "/?fields=description,status,summary,priority",
    Result    = httpRequest(Config, get, FullUrl),
    MapRes    = jsx:decode(Result, [return_maps]),
    Fields    = get(<<"fields">>, MapRes, #{}),
    Summary   = get(<<"summary">>, Fields, <<>>),
    Desc      = get(<<"description">>, Fields, Summary),
    Desc2     = characters_to_binary(Desc),
    Status    = get(<<"name">>, get(<<"status">>, Fields, #{}), <<>>),
    Priority  = get(<<"id">>, get(<<"priority">>, Fields, <<>>), 2),
    ComUrl    = BaseUrl ++ "/2/issue/" ++ Key ++ "/comment",
    Result2   = httpRequest(Config, get, ComUrl),
    MapRes2   = jsx:decode(Result2, [return_maps]),
    Comments1 = get(<<"comments">>, MapRes2, []),
    Comments2 = [{get(<<"created">>, Comment, <<>>),
                  get(<<"body">>,    Comment, <<>>),
                  get(<<"displayName">>,
                           get(<<"author">>, Comment, #{}), <<>>)}
                 || Comment <- Comments1],
    IssUrl     = "<" ++ maps:get(baseurl, Config) ++ "/browse/" ++ Key ++ ">",

    Todo = #todo{uid         = eTodoUtils:makeRef(),
                 status      = status2DB(Status),
                 priority    = prio2DB(Priority),
                 description = binary_to_list(Desc2),
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
    Msg  = <<Created/binary, " ", Author/binary, ": ", Body/binary, "\n">>,
    Msg2 = characters_to_binary(Msg),
    Acc2 = [binary_to_list(Msg2)|Acc],
    makeComment(Rest, Acc2);
makeComment([Value|Rest], Acc) when is_list(Value) ->
    makeComment(Rest, [Value ++ "\n"|Acc]);
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

defaultConfig() ->
    #{baseurl => "http://JIRA:8080/rest/api",
      user    => "",
      pwd     => "",
      search  => "project=\"CallGuide Development\" and "
                 "(issuetype=Story or issuetype=Task or issuetype=Sub-task) "
                 "and status not in (Resolved, Closed) and Assignee=currentUser()",
      bauth   => ""}.

