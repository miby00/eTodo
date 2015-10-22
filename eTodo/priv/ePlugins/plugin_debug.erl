%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_debug).

-export([getName/0, getDesc/0, getMenu/1, init/0, terminate/2]).

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

getName() -> "Debug plugin".

getDesc() -> "Prints function calls and arguments in console.".

-record(state, {callback = []}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init() -> #state{callback = init}.

%%--------------------------------------------------------------------
%% @doc
%% Free internal data for plugin.
%% @spec init() -> ok.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{callback = CBs}) ->
    io("terminate(State): ~p~n", [CBs]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Return key value list of right menu options.
%% Menu option should be a unique integer bigger than 1300.
%% @spec getMenu(ETodo) -> [{menuOption, menuText}, ...]
%% @end
%%--------------------------------------------------------------------
getMenu(_ETodo) -> [{60001, "Test menu"}].

%%--------------------------------------------------------------------
%% @doc
%% Called every 15 seconds to check if someone changes status
%% outside eTodo.
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       {ok, Status, StatusMsg, NewState}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(Dir, User, Status, StatusMsg, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eGetStatusUpdate|CBs]},
    io("eSetStatusUpdate(Dir, User, Status, StatusMsg, State): ~p~n",
       [[Dir, User, Status, StatusMsg, State2]]),
    {ok, Status, StatusMsg, State2}.

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(EScriptDir, User, Text, Hours, Min, Sec, State) ->
%%                     NewState
%% @end
%%--------------------------------------------------------------------
eTimerStarted(Dir, User, Text, Hours, Min, Sec, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eTimerStarted|CBs]},
    io("eTimerStarted(Dir, User, Text, Hours, Min, Sec, State): ~p~n",
        [[Dir, User, Text, Hours, Min, Sec, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(EScriptDir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerStopped(Dir, User, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eTimerStopped|CBs]},
    io("eTimerStopped(Dir, User, State): ~p~n",
        [[Dir, User, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% Timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerEnded(Dir, User, Text, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eTimerEnded|CBs]},
    io("eTimerEnded(Dir, User, Text, State): ~p~n",
        [[Dir, User, Text, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(Dir, User, Users, Text, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eReceivedMsg|CBs]},
    io("eReceivedMsg(Dir, User, Users, Text, State): ~p~n",
       [[Dir, User, Users, Text, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(Dir, Text, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eReceviedSysMsg|CBs]},
    io("eReceivedAlarmMsg(Dir, Text, State): ~p~n",
       [[Dir, Text, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(Dir, Text, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eReceivedAlarmMsg|CBs]},
    io("eReceivedAlarmMsg(Dir, Text, State): ~p~n",
       [[Dir, Text, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs in.
%%
%% @spec eLoggedInMsg(Dir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(Dir, User, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eLoggedInMsg|CBs]},
    io("eLoggedInMsg(Dir, User, State): ~p~n",
       [[Dir, User, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs out.
%%
%% @spec eLoggedOutMsg(Dir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(Dir, User, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eLoggedOutMsg|CBs]},
    io("eLoggedOutMsg(Dir, User, State): ~p~n", [[Dir, User, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(Dir, User, Status, StatusMsg, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eSetStatusUpdate|CBs]},
    io("eSetStatusUpdate(Dir, User, Status, StatusMsg, State): ~p~n",
       [[Dir, User, Status, StatusMsg, State2]]),
    State2.

%%--------------------------------------------------------------------
%% @doc
%% Called for right click menu
%%
%% @spec eMenuEvent(EScriptDir, User, MenuOption, ETodo, MenuText, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eMenuEvent(Dir, User, MenuOption, ETodo, MenuText, State = #state{callback = CBs}) ->
    State2 = State#state{callback = [eMenuEvent|CBs]},
    io("eMenuEvent(Dir, User, MenuOption, State): ~p~n",
       [[Dir, User, MenuOption, ETodo, MenuText, State2]]),
    State2.

io(Text, Args) ->
    io:format(Text, Args),
    ok.
