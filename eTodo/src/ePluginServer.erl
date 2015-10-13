%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael@TheExternal>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 25 Feb 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(ePluginServer).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([getInstalledPlugins/0,
         getConfiguredPlugins/0,
         setConfiguredPlugins/1,
         getRightMenuForPlugins/0]).

-export([eSetStatusUpdate/3,
         eGetStatusUpdate/3,
         eTimerStarted/5,
         eTimerStopped/1,
         eTimerEnded/2,
         eReceivedMsg/3,
         eReceivedSysMsg/1,
         eReceivedAlarmMsg/1,
         eLoggedInMsg/1,
         eLoggedOutMsg/1,
         eMenuEvent/3]).

-export([runCmd/3, runCmdGetResult/3]).

-define(SERVER, ?MODULE).

-record(state, {ePluginDir  = "",
                ePlugins    = [],
                eAllPlugins = [],
                ePluginMenu = []}).

-import(eTodoUtils, [makeStr/1, toStr/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all installed plugins.
%%
%% @spec getInstalledPlugins() -> [{FileName, Name, Desc}]
%% @end
%%--------------------------------------------------------------------
getInstalledPlugins() ->
    gen_server:call(?MODULE, getInstalledPlugins).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all configured plugins.
%%
%% @spec getConfiguredPlugins() -> [{FileName, Name, Desc}]
%% @end
%%--------------------------------------------------------------------
getConfiguredPlugins() ->
    gen_server:call(?MODULE, getConfiguredPlugins).

%%--------------------------------------------------------------------
%% @doc
%% Set configured plugins
%%
%% @spec setConfiguredPlugins(Plugins) -> ok
%% @end
%%--------------------------------------------------------------------
setConfiguredPlugins(Plugins) ->
    gen_server:cast(?MODULE, {setConfiguredPlugins, Plugins}).

%%--------------------------------------------------------------------
%% @doc
%% Get menu options for configured plugins
%%
%% @spec getRightMenuForPlugins() -> [{Name, [{MenuItem, MenuText}]}, ...]
%% @end
%%--------------------------------------------------------------------
getRightMenuForPlugins() ->
    gen_server:call(?MODULE, getRightMenuForPlugins).


%%--------------------------------------------------------------------
%% @doc
%% Called when the user changes status in eTodo
%%
%% @spec eSetStatusUpdate(User, Status, StatusMsg) -> noChange |
%%                                                    {ok, Status, StatusMsg}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(User, Status, StatusMsg) ->
    gen_server:call(?MODULE, {eGetStatusUpdate, [User, Status, StatusMsg]}).

%%--------------------------------------------------------------------
%% @doc
%% Called when the user changes status in eTodo
%%
%% @spec eSetStatusUpdate(User, Status, StatusMsg) -> ok
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(User, Status, StatusMsg) ->
    gen_server:cast(?MODULE, {eSetStatusUpdate, [User, Status, StatusMsg]}).

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(EScriptDir, User, Text, Hours, Min, Sec) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStarted(User, Text, Hours, Min, Sec) ->
    gen_server:cast(?MODULE, {eTimerStarted, [User, Text, Hours, Min, Sec]}).

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(User) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStopped(User) ->
    gen_server:cast(?MODULE, {eTimerStopped, [User]}).

%%--------------------------------------------------------------------
%% @doc
%% The timer ended
%%
%% @spec eTimerEnded(User, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerEnded(User, Text) ->
    gen_server:cast(?MODULE, {eTimerEnded, [User, Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eReceivedMsg() -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(User, Users, Text) ->
    gen_server:cast(?MODULE, {eReceivedMsg, [User, makeStr(Users), Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eReceivedSysMsg() -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(Text) ->
    gen_server:cast(?MODULE, {eReceivedSysMsg, [Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eReceivedAlarmMsg() -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(Text) ->
    gen_server:cast(?MODULE, {eReceivedAlarmMsg, [Text]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eLoggedInMsg() -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(User) ->
    gen_server:cast(?MODULE, {eLoggedInMsg, [User]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when a message arrives.
%%
%% @spec eLoggedOutMsg() -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(User) ->
    gen_server:cast(?MODULE, {eLoggedOutMsg, [User]}).

%%--------------------------------------------------------------------
%% @doc
%% ePlugin callback for when right click menu is used.
%%
%% @spec eMenuEvent() -> ok
%% @end
%%--------------------------------------------------------------------
eMenuEvent(User, MenuOption, ETodo) ->
    gen_server:cast(?MODULE, {eMenuEvent, [User, MenuOption, ETodo]}).

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
init([]) ->
    EPluginDir = ePluginDir(),
    EPlugins   = filelib:wildcard(EPluginDir ++ "/plugin*.beam"),
    Loaded     = (catch loadPlugins(EPlugins)),
    EPluginModules = [Module || {module, Module} <- Loaded],
    {ok, #state{ePluginDir  = ePluginDir(),
                ePlugins    = EPluginModules,
                eAllPlugins = EPluginModules}}.

loadPlugins(EPlugins) ->
    [code:load_abs(rootName(Filename)) || Filename <- EPlugins].

ePluginDir() ->
    filename:absname(filename:join([code:priv_dir(eTodo), "ePlugins"])).

rootName(Filename) ->
    filename:rootname(filename:absname(Filename)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages, calls are only made to plugin.erl
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(getInstalledPlugins, _From, State = #state{eAllPlugins = Plugins}) ->
    Reply = [{Plugin, Plugin:getName(), Plugin:getDesc()} || Plugin <- Plugins],
    {reply, Reply, State};
handle_call(getConfiguredPlugins, _From, State = #state{ePlugins = Plugins}) ->
    {reply, Plugins, State};
handle_call({Operation, Args}, From, State = #state{ePluginDir = Dir}) ->
    spawn(?MODULE, runCmdGetResult, [Operation, [Dir|Args], From]),
    {noreply, State};
handle_call(getRightMenuForPlugins, _From, State = #state{ePlugins = Plugins}) ->
    Menu = [{Plugin:getMenu(), Plugin} || Plugin <- Plugins,
                                          Plugin:getMenu() =/= []],
    Reply = [{{pluginName, Plugin:getName()}, Plugin:getMenu()} ||
                Plugin <- Plugins, Plugin:getMenu() =/= []],
    {reply, Reply, State#state{ePluginMenu = Menu}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages, casts are sent to every plugin.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({setConfiguredPlugins, Plugins}, State) ->
    {noreply, State#state{ePlugins = Plugins}};
handle_cast({eMenuEvent, Args = [_User, MenuOption, _ETodo]},
            State = #state{ePluginDir = Dir}) ->
    Modules = getModulesToCast(MenuOption, State#state.ePluginMenu),
    spawn(?MODULE, runCmd, [eMenuEvent, [Dir|Args], Modules]),
    {noreply, State};
handle_cast({Operation, Args}, State = #state{ePluginDir = Dir,
                                              ePlugins   = Modules}) ->
    spawn(?MODULE, runCmd, [Operation, [Dir|Args], Modules]),
    {noreply, State};
handle_cast(_Msg, State) ->
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
runCmd(_Operation, _Args, []) ->
    ok;
runCmd(Operation, Args, [EPlugin|Rest]) ->
    Result = (catch apply(EPlugin, Operation, Args)),
    eLog:log(debug, ?MODULE, runCmd, [Result],
             "Result from port program", ?LINE),
    runCmd(Operation, Args, Rest).

runCmdGetResult(Operation, Args, From) ->
    Result = (catch apply(plugin, Operation, Args)),
    eLog:log(debug, ?MODULE, runCmd, [Operation, Args, Result],
             "Result from port program", ?LINE),
    gen_server:reply(From, Result).

getModulesToCast(MenuOption, PluginMenuInfo) ->
    getModulesToCast(MenuOption, PluginMenuInfo, []).

getModulesToCast(_MenuOption, [], Acc) ->
    Acc;
getModulesToCast(MenuOption, [{MenuOptionsList, Module}|Rest], Acc) ->
    case lists:keymember(MenuOption, 1, MenuOptionsList) of
        true ->
            getModulesToCast(MenuOption, Rest, [Module | Acc]);
        false ->
            getModulesToCast(MenuOption, Rest, Acc)
    end.
