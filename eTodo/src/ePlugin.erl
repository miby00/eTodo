%%%-------------------------------------------------------------------
%%% @author mikael
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. okt 2015 21:02
%%%-------------------------------------------------------------------
-module(ePlugin).
-author("mikael.bylund@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop/1]).

-export([getMenu/2,
         eSetStatusUpdate/5,
         eGetStatusUpdate/5,
         eTimerStarted/7,
         eTimerStopped/3,
         eTimerEnded/4,
         eReceivedMsg/5,
         eReceivedSysMsg/3,
         eReceivedAlarmMsg/3,
         eLoggedInMsg/3,
         eLoggedOutMsg/3,
         eMenuEvent/6]).

-import(eTodoUtils, [makeStr/1, toStr/1]).

-record(state, {state, module}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(MODULE :: atom()) ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Module) ->
    gen_server:start_link(?MODULE, [Module], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Called when the user changes status in eTodo
%%
%% @spec getMenu(Pid, ETodo) -> Menu
%% @end
%%--------------------------------------------------------------------
getMenu(Pid, ETodo) ->
    gen_server:call(Pid, {getMenu, ETodo}).

%%--------------------------------------------------------------------
%% @doc
%% Called when the user changes status in eTodo
%%
%% @spec eSetStatusUpdate(Pid, Dir, User, Status, StatusMsg) ->
%%       noChange | {ok, Status, StatusMsg}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(Pid, Dir, User, Status, StatusMsg) ->
    gen_server:call(Pid, {eGetStatusUpdate, [Dir, User, Status, StatusMsg]}).
%%--------------------------------------------------------------------
%% @doc
%% Called when the user changes status in eTodo
%%
%% @spec eSetStatusUpdate(Pid, Dir, User, Status, StatusMsg) -> ok
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(Pid, Dir, User, Status, StatusMsg) ->
    gen_server:cast(Pid, {eSetStatusUpdate, [Dir, User, Status, StatusMsg]}).

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(Pid, EScriptDir, User, Text, Hours, Min, Sec) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStarted(Pid, Dir, User, Text, Hours, Min, Sec) ->
    gen_server:cast(Pid, {eTimerStarted, [User, Dir, Text, Hours, Min, Sec]}).

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(Pid, User) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStopped(Pid, Dir, User) ->
    gen_server:cast(Pid, {eTimerStopped, [Dir, User]}).

%%--------------------------------------------------------------------
%% @doc
%% The timer ended
%%
%% @spec eTimerEnded(Pid, Dir, User, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerEnded(Pid, Dir, User, Text) ->
    gen_server:cast(Pid, {eTimerEnded, [Dir, User, Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eReceivedMsg(Pid, Dir, User, Users, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(Pid, Dir, User, Users, Text) ->
    gen_server:cast(Pid, {eReceivedMsg, [Dir, User, makeStr(Users), Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eReceivedSysMsg(Pid, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(Pid, Dir, Text) ->
    gen_server:cast(Pid, {eReceivedSysMsg, [Dir, Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eReceivedAlarmMsg(Pid, Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(Pid, Dir, Text) ->
    gen_server:cast(Pid, {eReceivedAlarmMsg, [Dir, Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eLoggedInMsg(Pid, Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(Pid, Dir, User) ->
    gen_server:cast(Pid, {eLoggedInMsg, [Dir, User]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eLoggedOutMsg(Pid, Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(Pid, Dir, User) ->
    gen_server:cast(Pid, {eLoggedOutMsg, [Dir, User]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when right click menu is used.
%%
%% @spec eMenuEvent(Pid, Dir, User, MenuOption, ETodo, MenuText) -> ok
%% @end
%%--------------------------------------------------------------------
eMenuEvent(Pid, Dir, User, MenuOption, ETodo, MenuText) ->
    gen_server:cast(Pid, {eMenuEvent, [Dir, User, MenuOption, ETodo, MenuText]}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).

init([Module]) ->
    State = Module:init(),
    {ok, #state{state = State, module = Module}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_call({getMenu, ETodo}, _From,
    State = #state{state = PState, module = Module}) ->
    {Menu, State3} =
        case catch apply(Module, getMenu, [ETodo, PState]) of
            {ok, Menu2, PState2} ->
                {Menu2, State#state{state = PState2}};
            _ ->
                {[], State}
        end,

    eLog:log(debug, ?MODULE, runCmd, [getMenu, ETodo, Menu],
        "Result from port program", ?LINE),

    {reply, Menu, State3};
handle_call({eGetStatusUpdate, Args = [_User, Status, StatusMsg]}, _From,
            State = #state{state = PState, module = Module}) ->
    {Result2, State3} =
        case catch apply(Module, eGetStatusUpdate, Args ++ [PState]) of
            {ok, Status2, StatusMsg2, PState2} ->
                {{ok, Status2, StatusMsg2}, State#state{state = PState2}};
            _ ->
                {{ok, Status, StatusMsg}, State}
        end,

    eLog:log(debug, ?MODULE, runCmd, [eGetStatusUpdate, Args, Result2],
             "Result from port program", ?LINE),

    {reply, Result2, State3};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({Operation, Args},
             State = #state{state = PState, module = Module}) ->
    State3 =
        case catch apply(Module, Operation, Args ++ [PState]) of
            {'EXIT', _} ->
                State;
            PState2 ->
                State#state{state = PState2}
        end,
    {noreply, State3};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(Reason, State = #state{module = Module}) ->
    (catch Module:terminate(Reason, State)),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
