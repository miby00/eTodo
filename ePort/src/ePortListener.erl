%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, mikael
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(ePortListener).

-behaviour(gen_server).

%% API
-export([start_link/2,
    start_link/3,
    start_link/4,
    updateIPAllowed/2,
    updateListenPort/2,
    doAccept/5,
    disable/1,
    enable/1,
    stop/1]).

%% Utility functions
-export([getIpAddress/1
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ssl = false,
    listenSocket,
    module,
    allowedIps = all,
    disabled = false}).

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
start_link(Module, LPort) ->
    gen_server:start_link(?MODULE, [Module, LPort, all], []).

start_link(Module, LPort, AllowedIps) ->
    gen_server:start_link(?MODULE, [Module, LPort, AllowedIps], []).

start_link(Module, LPort, AllowedIps, SSLOptions) ->
    gen_server:start_link(?MODULE, [Module, LPort, AllowedIps, SSLOptions], []).

updateIPAllowed(Pid, IPAllowed) ->
    gen_server:cast(Pid, {updateIPAllowed, IPAllowed}).

updateListenPort(Pid, Port) ->
    gen_server:cast(Pid, {updateListenPort, Port}).

disable(Pid) ->
    gen_server:cast(Pid, disable).

enable(Pid) ->
    gen_server:cast(Pid, enable).

stop(Pid) ->
    gen_server:call(Pid, stop).

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
init([Module, LPort, AllowedIps]) ->
    eLog:log(debug, ?MODULE, init, [Module, LPort, AllowedIps],
        "Starting server...", ?LINE, Module),
    case gen_tcp:listen(LPort, [{reuseaddr, true},
        {active, false}, {packet, 4}]) of
        {ok, ListenSocket} ->
            self() ! nextWorker,
            {ok, #state{ssl          = false,
            listenSocket = ListenSocket,
            module       = Module,
            allowedIps   = AllowedIps}};
        {error, Reason} ->
            eLog:log(debug, ?MODULE, init, [LPort, Reason],
                "Cannot start listen port...", ?LINE, Module),
            {stop, Reason}
    end;
init([Module, LPort, AllowedIps, SSLOptions]) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),

    eLog:log(debug, ?MODULE, init, [Module, LPort, AllowedIps],
        "Starting server...", ?LINE, Module),

    case ssl:listen(LPort, [{reuseaddr, true},
        {active, false}, {packet, 4} | SSLOptions]) of
        {ok, ListenSocket} ->
            self() ! nextWorker,
            {ok, #state{ssl          = {true, SSLOptions},
            listenSocket = ListenSocket,
            module       = Module,
            allowedIps   = AllowedIps}};
        {error, Reason} ->
            eLog:log(debug, ?MODULE, init, [LPort, Reason],
                "Cannot start listen port...", ?LINE, Module),
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
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
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({updateIPAllowed, IPAllowed}, State) when is_list(IPAllowed) ->
    {noreply, State#state{allowedIps = IPAllowed}};
handle_cast({updateListenPort, Port},
    State = #state{ssl = false}) when is_integer(Port) ->
    gen_tcp:close(State#state.listenSocket),
    case catch gen_tcp:listen(Port, [{active, false}, {packet, 4}]) of
        {ok, ListenSocket} ->
            {noreply, State#state{listenSocket = ListenSocket}};
        Reason ->
            eLog:log(error, ?MODULE, handle_cast, [Reason],
                "Failed to open port... shutting down.", ?LINE,
                State#state.module),
            {stop, normal, State}
    end;
handle_cast({updateListenPort, Port},
    State = #state{ssl = {true, Options}}) when is_integer(Port) ->
    ssl:close(State#state.listenSocket),
    case catch ssl:listen(Port, [{active, false}, {packet, 4} | Options]) of
        {ok, ListenSocket} ->
            {noreply, State#state{listenSocket = ListenSocket}};
        Reason ->
            eLog:log(error, ?MODULE, handle_cast, [Reason],
                "Failed to open port... shutting down.", ?LINE,
                State#state.module),
            {stop, normal, State}
    end;
handle_cast(enable, State) ->
    self() ! nextWorker,
    {noreply, State#state{disabled = false}};
handle_cast(disable, State) ->
    {noreply, State#state{disabled = true}};
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
handle_info(_Info, State = #state{disabled = true}) ->
    {noreply, State};
handle_info(nextWorker, State = #state{ssl          = SSLConfig,
    module       = Module,
    listenSocket = ListenSocket,
    allowedIps   = AllowedIps}) ->

    spawn(?MODULE, doAccept, [self(), Module,
        ListenSocket, AllowedIps, SSLConfig]),
    {noreply, State};
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
terminate(Reason, #state{module = Module, listenSocket = LSocket}) ->
    eLog:log(debug, ?MODULE, terminate, [Reason, LSocket],
        "Shutting down...", ?LINE, Module),
    (catch gen_tcp:close(LSocket)),
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
doAccept(LPid, Module, ListenSocket, AllowedIps, false) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            startPort(LPid, Module, Socket, AllowedIps, false);
        Reason ->
            eLog:log(debug, ?MODULE, doAccept, [Reason],
                "Received error from accept", ?LINE, Module),
            Reason
    end,
    LPid ! nextWorker;
doAccept(LPid, Module, ListenSocket, AllowedIps, {true, SSLOptions}) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case catch ssl:ssl_accept(Socket) of
                ok ->
                    startPort(LPid, Module, Socket, AllowedIps, {true, SSLOptions});
                _RetValue ->
                    ok
            end;
        Reason ->
            eLog:log(debug, ?MODULE, doAccept, [Reason],
                "Received error from accept", ?LINE, Module),
            Reason
    end,
    LPid ! nextWorker.

startPort(LPid, Module, Socket, AllowedIps, false) ->
    case allowedIp(Socket, AllowedIps) of
        {true, IP} ->
            eLog:log(debug, ?MODULE, startPort, [IP],
                "Connect attempt", ?LINE, Module),
            case ePort:start(LPid, Module, Socket) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid);
                Reason ->
                    Reason
            end;
        {false, IP} ->
            eLog:log(debug, ?MODULE, startPort, [IP],
                "Illegal connect attempt", ?LINE, Module),
            gen_tcp:close(Socket),
            {error, notAllowedIp}
    end;
startPort(LPid, Module, Socket, AllowedIps, {true, SSLOptions}) ->
    case allowedIp(Socket, AllowedIps) of
        {true, IP} ->
            eLog:log(debug, ?MODULE, startPort, [IP],
                "Connect attempt", ?LINE, Module),
            case ePort:start(LPid, Module, Socket, SSLOptions) of
                {ok, Pid} ->
                    ssl:controlling_process(Socket, Pid);
                Reason ->
                    Reason
            end;
        {false, IP} ->
            eLog:log(debug, ?MODULE, startPort, [IP],
                "Illegal connect attempt", ?LINE, Module),
            ssl:close(Socket),
            {error, notAllowedIp}
    end.

allowedIp(Socket,  AllowedIps) when is_list(AllowedIps)->
    IP = getIpAddress(Socket),
    {lists:member(IP, AllowedIps), IP};
allowedIp(Socket,  _) ->
    IP = getIpAddress(Socket),
    {true, IP}.

getIpAddress(Socket) ->
    case catch inet:peername(Socket) of
        {ok, {Address, _}} ->
            inet_parse:ntoa(Address);
        _ ->
            case catch ssl:peername(Socket) of
                {ok, {Address, _}} ->
                    inet_parse:ntoa(Address);
                _ ->
                    %% No address of client was detected.
                    undefined
            end
    end.