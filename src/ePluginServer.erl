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
         getRightMenuForPlugins/1]).

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

-export([runCmd/4, runCmd/3, runCmdGetResult/4]).

-define(SERVER, ?MODULE).

-record(state, {ePluginDir     = "",
                ePlugins       = [],
                eAllPlugins    = [],
                ePluginMenu    = [],
                ePluginServers = []}).

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
%% @spec getRightMenuForPlugins(ETodo) -> [{Name, [{MenuItem, MenuText}]}, ...]
%% @end
%%--------------------------------------------------------------------
getRightMenuForPlugins(ETodo) ->
    gen_server:call(?MODULE, {getRightMenuForPlugins, ETodo}).


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
    EPluginServers = [{Module, ePlugin:start_link(Module)} || Module <- EPluginModules],
    {ok, #state{ePluginDir     = ePluginDir(),
                ePlugins       = EPluginModules,
                eAllPlugins    = EPluginModules,
                ePluginServers = EPluginServers}}.

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
handle_call({getRightMenuForPlugins, ETodo},
    _From, State = #state{ePlugins = Plugins, ePluginServers = Servers}) ->
    Menu = [{getMenu(Plugin, ETodo, Servers), Plugin} || Plugin <- Plugins,
        getMenu(Plugin, ETodo, Servers) =/= []],
    Reply = [{{pluginName, Plugin:getName()}, getMenu(Plugin, ETodo, Servers)} ||
        Plugin <- Plugins, getMenu(Plugin, ETodo, Servers) =/= []],
    {reply, Reply, State#state{ePluginMenu = Menu}};
handle_call({Operation, Args}, From, State = #state{ePluginDir     = Dir,
                                                    ePluginServers = Servers}) ->
    spawn(?MODULE, runCmdGetResult, [Operation, [Dir|Args], From, Servers]),
    {noreply, State};
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
handle_cast({setConfiguredPlugins, Plugins},
             State = #state{ePlugins       = Old,
                            ePluginServers = Servers}) ->

    ModulesToStop  = Old -- Plugins,
    ModulesToStart = Plugins -- Old,

    Servers2 = stopPluginServers(ModulesToStop,   Servers),
    Servers3 = startPluginServers(ModulesToStart, Servers2),

    {noreply, State#state{ePlugins = Plugins, ePluginServers = Servers3}};
handle_cast({eMenuEvent, Args = [_User, MenuOption, _ETodo]},
            State = #state{ePluginDir = Dir, ePluginServers = Servers}) ->
    ModTuple = getModulesToCast(MenuOption, State#state.ePluginMenu),
    spawn(?MODULE, runCmd, [eMenuEvent, [Dir|Args], Servers, ModTuple]),
    {noreply, State};
handle_cast({Operation, Args}, State = #state{ePluginDir     = Dir,
                                              ePluginServers = Servers}) ->
    spawn(?MODULE, runCmd, [Operation, [Dir|Args], Servers]),
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
runCmd(_Operation, _Args, _Servers, []) ->
    ok;
runCmd(Operation, Args, Servers, [{EPlugin, Arg}|Rest]) ->
    case lists:keyfind(EPlugin, 1, Servers) of
        {EPlugin, {ok, Pid}} ->
            (catch apply(ePlugin, Operation, [Pid|Args] ++ [Arg]));
        _ ->
            ok
    end,
    runCmd(Operation, Args, Servers, Rest).


runCmd(_Operation, _Args, []) ->
    ok;
runCmd(Operation, Args, [{_Module, {ok, Pid}}|Rest]) ->
    (catch apply(ePlugin, Operation, [Pid|Args])),
    runCmd(Operation, Args, Rest);
runCmd(Operation, Args, [_StartupError|Rest]) ->
    runCmd(Operation, Args, Rest).

runCmdGetResult(eGetStatusUpdate, [_User, Status, StatusMsg], From, []) ->
    gen_server:reply(From, {ok, Status, StatusMsg});
runCmdGetResult(eGetStatusUpdate, [User, Status, StatusMsg], From,
                [{Module, {ok, Pid}}|Rest]) ->

    {Status3, StatusMsg3} =
        case catch apply(Module, eGetStatusUpdate,
                         [Pid, User, Status, StatusMsg]) of
            {ok, Status2, StatusMsg2} ->
                {Status2, StatusMsg2};
            _ ->
                {Status, StatusMsg}
        end,
    runCmdGetResult(eGetStatusUpdate, [User, Status3, StatusMsg3], From, Rest).

getModulesToCast(MenuOption, PluginMenuInfo) ->
    getModulesToCast(MenuOption, PluginMenuInfo, []).

getModulesToCast(_MenuOption, [], Acc) ->
    Acc;
getModulesToCast(MenuOption, [{MenuOptionsList, Module}|Rest], Acc) ->
    case checkModule(MenuOption, MenuOptionsList, Module) of
        {MenuOption, MenuText} ->
            getModulesToCast(MenuOption, Rest, [{Module, MenuText} | Acc]);
        false ->
            getModulesToCast(MenuOption, Rest, Acc)
    end.

checkModule(_MenuOption, [], _Module) ->
    false;
checkModule(MenuOption, [{{subMenu, _}, MenuOptions}|Rest], Module) ->
    case checkModule(MenuOption, MenuOptions, Module) of
        {MenuOption, MenuText} ->
            {MenuOption, MenuText};
        false ->
            checkModule(MenuOption, Rest, Module)
    end;
checkModule(MenuOption, [{MenuOption, MenuText}|_Rest], _Module) ->
    {MenuOption, MenuText};
checkModule(MenuOption, [{_MenuOption, _}|Rest], Module) ->
    checkModule(MenuOption, Rest, Module).

stopPluginServers([], Servers) ->
    Servers;
stopPluginServers([Module|Rest], Servers) ->
    case lists:keytake(Module, 1, Servers) of
        {value, {Module, {ok, Pid}}, Servers2} ->
            ePlugin:stop(Pid),
            stopPluginServers(Rest, Servers2);
        _ ->
            stopPluginServers(Rest, Servers)
    end.

startPluginServers([], Servers) ->
    Servers;
startPluginServers([Module|Rest], Servers) ->
    Servers2 = [{Module, ePlugin:start_link(Module)}|Servers],
    startPluginServers(Rest, Servers2).

getMenu(_Plugin, _ETodo, []) ->
    [];
getMenu(Plugin, ETodo, [{Plugin, {ok, Pid}}|_Rest]) ->
    ePlugin:getMenu(Pid, ETodo);
getMenu(Plugin, ETodo, [{_Plugin, _}|Rest]) ->
    getMenu(Plugin, ETodo, Rest).
