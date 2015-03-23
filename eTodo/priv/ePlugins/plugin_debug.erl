%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_debug).

-export([getName/0, getDesc/0]).

-export([eGetStatusUpdate/4,
         eTimerStarted/6,
         eTimerStopped/2,
         eTimerEnded/3,
         eReceivedMsg/4,
         eReceivedSysMsg/2,
         eReceivedAlarmMsg/2,
         eLoggedInMsg/2,
         eLoggedOutMsg/2,
         eSetStatusUpdate/4]).

getName() -> "Debug plugin".

getDesc() -> "Prints function calls and arguments in console.".

%% Calls are only made to plugin.beam

%%--------------------------------------------------------------------
%% @doc
%% Called every 15 seconds to check if someone changes status
%% outside eTodo.
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg) ->
%%       {ok, Status, StatusMsg}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(Dir, User, Status, StatusMsg) ->
    io("eSetStatusUpdate(Dir, User, Status, StatusMsg): ~p~n",
       [[Dir, User, Status, StatusMsg]]),
    {ok, Status, StatusMsg}.

%% Casts are made to all plugin*.beam

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(EScriptDir, User, Text, Hours, Min, Sec) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStarted(Dir, User, Text, Hours, Min, Sec) ->
    io("eTimerStarted(Dir, User, Text, Hours, Min, Sec): ~p~n",
        [[Dir, User, Text, Hours, Min, Sec]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(EScriptDir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStopped(Dir, User) ->
    io("eTimerStopped(Dir, User): ~p~n",
        [[Dir, User]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerEnded(Dir, User, Text) ->
    io("eTimerEnded(Dir, User, Text): ~p~n",
        [[Dir, User, Text]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(Dir, User, Users, Text) ->
    io("eReceivedMsg(Dir, User, Users, Text): ~p~n",
       [[Dir, User, Users, Text]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(Dir, Text) ->
    io("eReceivedAlarmMsg(Dir, Text): ~p~n",
       [[Dir, Text]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(Dir, Text) ->
    io("eReceivedAlarmMsg(Dir, Text): ~p~n",
       [[Dir, Text]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs in.
%%
%% @spec eLoggedInMsg(Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(Dir, User) ->
    io("eLoggedInMsg(Dir, User): ~p~n",
       [[Dir, User]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs out.
%%
%% @spec eLoggedOutMsg(Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(Dir, User) ->
    io("eLoggedOutMsg(Dir, User): ~p~n", [[Dir, User]]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg) -> ok
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(Dir, User, Status, StatusMsg) ->
    io("eSetStatusUpdate(Dir, User, Status, StatusMsg): ~p~n",
       [[Dir, User, Status, StatusMsg]]),
    ok.

io(_Text, _Args) ->
    %% Uncomment to make IO
    io:format(_Text, _Args),
    ok.
