%%%-------------------------------------------------------------------
%%% @Author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_notify).

-export([getName/0, getDesc/0]).

-export([eReceivedMsg/4,
         eReceivedSysMsg/2,
         eReceivedAlarmMsg/2,
         eTimerEnded/3,
         eLoggedInMsg/2,
         eLoggedOutMsg/2,
         eSetStatusUpdate/4]).

getName() -> "Notify".

getDesc() -> "Plugin which handles notifications.".

%% Casts are made to all plugin*.beam

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(EScriptDir, User, Users, Text) ->
    notify(os:type(), EScriptDir, User, Users, Text).

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(_Dir, _Text) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerEnded(EScriptDir, _User, Text) ->
    notify(os:type(), EScriptDir, "eTodo", "eTodo", Text),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(_Dir, _Text) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs in.
%%
%% @spec eLoggedInMsg(Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(_Dir, _User) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs out.
%%
%% @spec eLoggedOutMsg(Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(_Dir, _User) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg) -> ok
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(_Dir, _User, _Status, _StatusMsg) ->
    ok.

notify({unix, linux}, EScriptDir, User, Users, Text) ->
    %% Text2 = binary_to_list(unicode:characters_to_binary(Text, utf8)),
    Cmd  = "notify-send -u critical -i \"" ++ icon(EScriptDir) ++ "\" "
        "\"" ++ User ++ " -> " ++ Users ++ "\" \"" ++ Text ++ "\"",
    os:cmd(Cmd);
notify({unix, darwin}, EScriptDir, User, Users, Text) ->
    %% Text2 = binary_to_list(unicode:characters_to_binary(Text, utf8)),
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
