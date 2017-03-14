%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2017 20:25
%%%-------------------------------------------------------------------
-module(eTodoNoGui).

-author("mikael.bylund@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         getSearchCfg/0,
         getFilterCfg/1,
         getTimeReportData/0,
         getTaskList/0]).

-export([loggedIn/1,
         taskListDeleted/1,
         todoDeleted/1,
         todoUpdated/2,
         updateTaskList/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, eTodo).

-include_lib("eTodo/include/eTodo.hrl").

-import(eGuiFunctions, [getPortrait/1,
                        getTodoLists/1,
                        useFilter/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

getSearchCfg() ->
    gen_server:call(?SERVER, getSearchCfg).

getTaskList() ->
    gen_server:call(?SERVER, getTaskList).

getFilterCfg(TaskList) ->
    gen_server:call(?SERVER, {getFilterCfg, TaskList}).

getTimeReportData() ->
    gen_server:call(?SERVER, getTimeReportData).

updateTaskList(TaskList) ->
    gen_server:cast(?SERVER, {updateTaskList, TaskList}).

todoUpdated(Sender, Todo) ->
    gen_server:cast(?SERVER, {todoUpdated, Sender, Todo}), ok.

loggedIn(User) ->
    gen_server:cast(?SERVER, {loggedIn, User}), ok.

todoDeleted(Uid) ->
    gen_server:cast(?SERVER, {todoDeleted, Uid}), ok.

taskListDeleted(List) ->
    gen_server:cast(?SERVER, {taskListDeleted, List}), ok.

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
             {ok, State :: #guiState{}} | {ok, State :: #guiState{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, User}   = application:get_env(eTodo, user),
    {ok, Pwd}    = application:get_env(eTodo, pwd),
    {ok, Circle} = application:get_env(eTodo, circle),
    %% Add handler to receive events
    ePeerEM:add_handler(eTodoEH, {User, noGui}),
    ePeerEM:connectToCircle(User, Circle, Pwd),
    eTodoAlarm:loggedIn(User),
    {ok, #guiState{mode = noGui, taskList = ?defTaskList}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #guiState{}) ->
             {reply, Reply :: term(), NewState :: #guiState{}} |
             {reply, Reply :: term(), NewState :: #guiState{}, timeout() | hibernate} |
             {noreply, NewState :: #guiState{}} |
             {noreply, NewState :: #guiState{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #guiState{}} |
             {stop, Reason :: term(), NewState :: #guiState{}}).
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(getTaskList, _From, State) ->
    TaskList = State#guiState.taskList,
    {reply, TaskList, State};
handle_call(getSearchCfg, _From, State = #guiState{searchCfg = Cfg}) ->
    {reply, Cfg, State};
handle_call({getFilterCfg, TaskList}, _From, State = #guiState{filter = Cfg}) ->
    Filter = useFilter(TaskList, Cfg, State),
    {reply, Filter, State};
handle_call(getTimeReportData, _From, State) ->
    TaskList = State#guiState.taskList,
    AllTask  = TaskList == ?defTaskList,
    Reply    = {ok, State#guiState.rows, AllTask},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #guiState{}) ->
             {noreply, NewState :: #guiState{}} |
             {noreply, NewState :: #guiState{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #guiState{}}).
handle_cast({todoUpdated, _Sender, Todo = #todo{}},
            State = #guiState{mode = noGui,
                              user = User}) ->
    NewTodo = eTodoDB:newTodo(Todo),
    eTodoDB:updateTodoNoLocks(Todo),
    case NewTodo of
        true ->
            Row = eTodoDB:getRow(User, ?defTaskList),
            eTodoDB:addTodo(#userInfo{userName = User,
                                      uid      = Todo#todo.uid,
                                      row      = Row,
                                      parent   = ?defTaskList}, Todo);
        false ->
            ok
    end,
    {noreply, State};
handle_cast({todoUpdated, _Sender, Todo = #diff{}},
            State = #guiState{mode = noGui}) ->
    case eTodoDB:newTodo(Todo) of
        true ->
            %% Shouldn't be a diff, this is a removed eTodo, ignore.
            ok;
        false ->
            eTodoDB:updateTodoNoLocks(Todo)
    end,
    {noreply, State};
handle_cast({loggedIn, Peer}, State = #guiState{mode = noGui,
                                                user = User}) ->
    case getStatus() of
        {Status, StatusMsg} when
              (Status =/= false) and (StatusMsg =/= false) ->
            StatusUpdate = #userStatus{userName  = User,
                                       status    = Status,
                                       statusMsg = StatusMsg},
            eWeb:setStatusUpdate(User, Status, StatusMsg),
            ePeerEM:sendMsg(User, [Peer], statusEntry,
                            {statusUpdate, StatusUpdate, getPortrait(User)});
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({todoDeleted, Uid},
            State = #guiState{user = User, mode = noGui}) ->
    %% Remove task from database.
    eTodoDB:delTodo(Uid, User),
    {noreply, State};
handle_cast({taskListDeleted, List},
            State = #guiState{user = User, mode = noGui}) ->
    TodoLists1 = getTodoLists(User),
    TodoLists2 = lists:delete(List, TodoLists1),
    eTodoDB:moveTodosToTaskList(User, ?defTaskList, List),
    UserCfg = eTodoDB:readUserCfg(User),
    eTodoDB:saveUserCfg(UserCfg#userCfg{lists = TodoLists2}),
    {noreply, State};
handle_cast({updateTaskList, TaskList}, State = #guiState{mode = noGui}) ->
    {noreply, State#guiState{taskList = TaskList}};
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
-spec(handle_info(Info :: timeout() | term(), State :: #guiState{}) ->
             {noreply, NewState :: #guiState{}} |
             {noreply, NewState :: #guiState{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #guiState{}}).
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
                State :: #guiState{}) -> term()).
terminate(_Reason, #guiState{mode = noGui}) ->
    init:stop(),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #guiState{},
                  Extra :: term()) ->
             {ok, NewState :: #guiState{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%====================================================================
%% Get status configuration
%%====================================================================
getStatus() ->
    case application:get_env(eTodo, status) of
        {ok, Status} ->
            case application:get_env(eTodo, statusMsg) of
                {ok, StatusMsg} ->
                    {Status, StatusMsg};
                _ ->
                    {Status, false}
            end;
        _ ->
            {false, false}
    end.

