%%%-------------------------------------------------------------------
%%% @Author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_notify).

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

getName() -> "Notify".

getDesc() -> "Plugin which handles notifications.".

-record(state, {}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init(_Args) -> #state{}.

%%--------------------------------------------------------------------
%% @doc
%% Free internal data for plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Return key value list of right menu options.
%% Menu option should be a unique integer bigger than 1300.
%% @spec getMenu(ETodo, State) -> {ok, [{menuOption, menuText}, ...], NewState}
%% @end
%%--------------------------------------------------------------------
getMenu(_ETodo, State) -> {ok, [], State}.

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
eTimerStarted(_Dir, _User, _Text, _Hours, _Min, _Sec, State) ->
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
%% Timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerEnded(EScriptDir, _User, Text, State) ->
    notify(os:type(), EScriptDir, "eTodo", "eTodo", Text),
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(EScriptDir, User, Users, Text, State) ->
    notify(os:type(), EScriptDir, User, Users, Text),
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
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       NewState
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
%%       NewState
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, _User, _MenuOption, _ETodo, _MenuText, State) ->
    State.

notify({unix, linux}, EScriptDir, User, Users, Text) ->
    Cmd  = "notify-send -u critical -i \"" ++ icon(EScriptDir) ++ "\" "
    "\"" ++ User ++ " -> " ++ Users ++ "\" \"" ++ Text ++ "\"",
    os:cmd(Cmd);
notify({unix, darwin}, EScriptDir, User, Users, Text) ->
    Cmd = "\"" ++ EScriptDir ++ "/terminal-notifier.app/"
    "Contents/MacOS/terminal-notifier\" -message \"" ++ Text ++
        "\" -title \"eTodo " ++ User ++ " -> " ++ Users ++ "\"",
    os:cmd(Cmd);
notify({win32, _}, EScriptDir, User, Users, Text) ->
    addToPath(EScriptDir),
    Cmd  = "notifu /i \"" ++ icon(EScriptDir) ++ "\" "
    "/p \"" ++ User ++ " -> " ++ Users ++ "\" /m \"" ++ Text ++ "\" /d 5",
    os:cmd(Cmd);
notify(_OS, _Dir, _User, _Users, _Text) ->
    ok.

icon(Dir) ->
    filename:join([Dir, "..", "Icons", "ETodo.ico"]).

addToPath(Dir) ->
    WinPath = os:getenv("PATH"),
    os:putenv("PATH", filename:nativename(Dir) ++ ";" ++ WinPath).

