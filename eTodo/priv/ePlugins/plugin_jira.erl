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

-record(etodo,  {status,
                 statusCol,
                 statusDB,
                 priority,
                 priorityCol,
                 priorityDB,
                 owner,
                 ownerCol,
                 dueTime,
                 dueTimeCol,
                 dueTimeDB,
                 description,
                 descriptionCol,
                 comment,
                 commentCol,
                 sharedWith,
                 sharedWithCol,
                 sharedWithDB,
                 createTime,
                 createTimeCol,
                 createTimeDB,
                 doneTime,
                 doneTimeCol,
                 doneTimeDB,
                 hasSubTodo,
                 uid,
                 uidCol,
                 uidDB,
                 progress,
                 lists,
                 listsDB}).

getName() -> "JIRA".

getDesc() -> "Create JIRA task from eTodo.".

-record(config, {baseurl = "http://jira.upp.telia.se:8080/rest/api",
                 user    = "miby00",
                 pwd     = "miby00",
                 bauth   = ""}).

-record(state, {config = #config{}}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init() ->
    Config = case file:consult("jira.config") of
                 {ok, Cfg} when is_record(Cfg, config) ->
                     Cfg;
                 _ ->
                     #config{}
             end,
    UserPwd     = Config#config.user ++ ":" ++ Config#config.pwd,
    BAuth       = "Basic " ++ base64:encode_to_string(UserPwd),
    Config2     = Config#config{bauth = BAuth},
    #state{config = Config2}.

httpRequest(#config{bauth = BAuth}, Method, Url) ->
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

%%--------------------------------------------------------------------
%% @doc
%% Free internal data for plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{config = Cfg}) ->
    case filelib:is_file("jira.config") of
        true ->
            ok;
        false ->
            file:write_file("jira.config", io_lib:format("~tp.~n", [Cfg]))
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Return key value list of right menu options.
%% Menu option should be a unique integer bigger than 1300.
%% @spec getMenu(ETodo, State) -> {ok, [{menuOption, menuText}, ...], NewState}
%% @end
%%--------------------------------------------------------------------
getMenu(_ETodo, State) -> {ok, [{55000, "Create JIRA Task"},
                                {55001, "Update JIRA Task"},
                                {{subMenu, "Create Task from feature"},
                                 [{55002, "Feature 1"},
                                  {55003, "Feature 2"},
                                  {55004, "Feature 3"}]}], State}.

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
eMenuEvent(_EScriptDir, _User, 55000, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    Config = State#state.config,
    Search = "project=\"CallGuide Feature\" AND issuetype=\"CallGuide Feature\" AND status not in (RestrictedRelease, OnHold, GeneralAvailability, Closed) AND \"Fea. ConstrLeader\"=miby00",
    Url    = Config#config.baseurl ++ "/2/search?jql=" ++ http_uri:encode(Search),
    Result = httpRequest(Config, get, Url),
    State;
eMenuEvent(_EScriptDir, _User, 55001, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    State;
eMenuEvent(_EScriptDir, _User, 55002, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    State;
eMenuEvent(_EScriptDir, _User, 55003, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    State;
eMenuEvent(_EScriptDir, _User, 55004, _ETodo, _MenuText, State) ->
    io:format(_MenuText),
    State;
eMenuEvent(_EScriptDir, _User, _MenuOption, _ETodo, _MenuText, State) ->
    io:format(integer_to_list(_MenuOption)),
    State.
