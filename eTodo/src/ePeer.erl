%%%-------------------------------------------------------------------
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @author Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%%
%%% Changed:  28 Juni 2013 Mikael Bylund rewrote to avoid deadlock.
%%%-------------------------------------------------------------------
-module(ePeer).

-behaviour(gen_server).

-include("eTodo.hrl").

%% API
-export([start/3,
         stop/1,
         peerCall/4,
         peerCast/3,
         sendMsg/5,
         todoUpdated/3,
         getLock/3,
         releaseLock/3,
         webProxyCall/3,
         proxyCall/5,
         proxyCall/6,
         proxyCast/5]).

%% Local call progress
-export([doConnect/0,
         doRegister/0,
         doProxyLogin/0,
         doWebProxyCall/3,
         doWebProxyCall/5,
         doSpawnAndReturn/4,
         sendCall/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SendPingDelay, 10 * 1000).
-define(RecvPingDelay, 20 * 1000).

-record(state, {stateName, %% See below.
                stateData,
                hashPwd,
                parent,
                peerUser,
                peerHost,
                peerPort,
                user,
                host,
                port,
                circle,
                ePortPid,
                eListenerPid,
                proxyPid,
                proxyRef,
                sendPingTimer,
                recvPingTimer
               }).

-import(eTodoUtils, [default/2]).

%%--------------------------------------------------------------------
%% StateName sequence when initiating a direct connection:
%%    connectToPeer -> registerWithPeer -> authorized -> ...
%% StateName sequence when receiving a direct connection:
%%    clientConnected -> connected -> authorized -> ...
%% StateName sequence when initiating a proxy connection:
%%    proxyToPeer -> connectToPeer -> registerWithPeer -> authorized -> ...
%% StateName sequence when receiving a proxy connection:
%%    clientConnected -> connected -> authorized -> ...
%%--------------------------------------------------------------------

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(Parent, Event, Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Parent, Event, Args) ->
    gen_server:start(?MODULE, [Parent, Event, Args], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @spec stop(Pid) -> ok
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Handle call from a another peer.
%% @spec peerMessage(Pid, From, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
peerCall(Pid, From, Message, Args) ->
    gen_server:cast(Pid, {peerCall, From, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Handle cast from a another peer.
%% @spec peerMessage(Pid, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
peerCast(Pid, Message, Args) ->
    gen_server:cast(Pid, {peerCast, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Send chat msg to peer.
%% @spec sendMsg(Pid, User, PeerUsers, MsgType, Text) -> ok
%% @end
%%--------------------------------------------------------------------
sendMsg(Pid, User, PeerUsers, MsgType, Text) ->
    gen_server:cast(Pid, {sendMsg, User, PeerUsers, MsgType, Text}).

%%--------------------------------------------------------------------
%% @doc
%% Update task for user.
%% @spec todoUpdated(Pid, PeerUser, Task) -> ok
%% @end
%%--------------------------------------------------------------------
todoUpdated(Pid, PeerUser, Task) ->
    gen_server:cast(Pid, {todoUpdated, PeerUser, Task}).

%%--------------------------------------------------------------------
%% @doc
%% Get lock
%% @spec getLock(Pid, Uid, Owner) -> ok
%% @end
%%--------------------------------------------------------------------
getLock(Pid, Uid, Owner) ->
    gen_server:call(Pid, {getLock, Uid, Owner}).

%%--------------------------------------------------------------------
%% @doc
%% Release lock
%% @spec releaseLock(Pid, Uid, Owner) -> ok
%% @end
%%--------------------------------------------------------------------
releaseLock(Pid, Uid, Owner) ->
    gen_server:cast(Pid, {releaseLock, Uid, Owner}).

%%--------------------------------------------------------------------
%% @doc
%% Get lock
%% @spec webProxyCall(Pid, Message, Timeout) -> Result
%% @end
%%--------------------------------------------------------------------
webProxyCall(Pid, Message, Timeout) ->
    gen_server:call(Pid, {webProxyCall, Message, Timeout}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Proxy message from other ePeer.
%% @spec proxyCall(Pid, SrcUser, DestUser, Message, Args, Timeout) -> ok
%% @end
%%--------------------------------------------------------------------
proxyCall(Pid, SrcUser, DestUser, Message, Args) ->
    proxyCall(Pid, SrcUser, DestUser, Message, Args, 5000).

proxyCall(Pid, SrcUser, DestUser, Message, Args, Timeout) ->
    gen_server:call(Pid,
                    {proxyCall, SrcUser, DestUser, Message, Args}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Proxy message from other ePeer.
%% @spec proxyCast(Pid, SrcUser, DestUser, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
proxyCast(Pid, SrcUser, DestUser, Message, Args) ->
    gen_server:cast(Pid, {proxyCast, SrcUser, DestUser, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Start connecting to remote peer.
%% @spec doConnect() -> ok
%% @end
%%--------------------------------------------------------------------
doConnect() ->
    gen_server:cast(self(), doConnect).

%%--------------------------------------------------------------------
%% @doc
%% Reigster with remote peer.
%% @spec doRegister() -> ok
%% @end
%%--------------------------------------------------------------------
doRegister() ->
    gen_server:cast(self(), doRegister).

%%--------------------------------------------------------------------
%% @doc
%% Send login to remote peer.
%% @spec doProxyLogin() -> ok
%% @end
%%--------------------------------------------------------------------
doProxyLogin() ->
    gen_server:cast(self(), doProxyLogin).

%%%===================================================================
%%% EPort API
%%%===================================================================
sendCall(Message, Args, Timeout, #state{proxyPid = undefined,
                                        ePortPid = EPortPid}) ->
    ePort:call(EPortPid, Message, Args, Timeout);
sendCall(Message, Args, Timeout, #state{proxyPid = ProxyPid,
                                        peerUser = PeerUser,
                                        user     = User}) ->
    ePeer:proxyCall(ProxyPid, User, PeerUser, Message, Args, Timeout).

sendCall(Message, Args, #state{proxyPid = undefined,
                               ePortPid = EPortPid}) ->
    ePort:call(EPortPid, Message, Args);
sendCall(Message, Args, #state{proxyPid = ProxyPid,
                               peerUser = PeerUser,
                               user     = User}) ->
    ePeer:proxyCall(ProxyPid, User, PeerUser, Message, Args).

sendCast(Message, Args, #state{proxyPid = undefined,
                               ePortPid = EPortPid}) ->
    ePort:cast(EPortPid, Message, Args);
sendCast(Message, Args, #state{proxyPid = ProxyPid,
                               peerUser = PeerUser,
                               user     = User}) ->
    ePeer:proxyCast(ProxyPid, User, PeerUser, Message, Args).

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
init([Parent, Event, Args]) ->
    process_flag(trap_exit, true),
    State = doInit(Parent, Event, Args),
    eLog:log(debug, ?MODULE, init, [self(), State],
             "started", ?LINE),
    UserCfg  = eTodoDB:readUserCfg(State#state.user),
    Password = default(UserCfg#userCfg.webPassword, ""),
    HashPwd  = crypto:hash(sha, Password),
    {ok, State#state{hashPwd = HashPwd}}.

doInit(Parent, connectToPeer, Args) ->
    {EPortPid, PeerUser, PeerHost, PeerPort,
     User, Port, Circle} = Args,
    doConnect(),
    link(EPortPid),
    #state{stateName    = connectToPeer,
           parent       = Parent,
           ePortPid     = EPortPid,
           peerUser     = PeerUser,
           peerHost     = PeerHost,
           peerPort     = PeerPort,
           user         = User,
           port         = Port,
           circle       = Circle};
doInit(Parent, clientConnected, Args) ->
    {EPortPid, EListenerPid, PeerHost, Circle, User} = Args,
    link(EPortPid),
    #state{stateName    = clientConnected,
           parent       = Parent,
           peerHost     = PeerHost,
           user         = User,
           circle       = Circle,
           ePortPid     = EPortPid,
           eListenerPid = EListenerPid};
doInit(Parent, proxyToPeer, Args) ->
    {PeerUser, ProxyPid, User, Circle} = Args,
    eLog:log(debug, ?MODULE, proxyToPeer,
             [self(), PeerUser, ProxyPid, User],
             "Proxy ePeer starting", ?LINE),
    doProxyLogin(),
    Reference = erlang:monitor(process, ProxyPid),
    #state{stateName = proxyToPeer,
           parent    = Parent,
           peerUser  = PeerUser,
           proxyPid  = ProxyPid,
           proxyRef  = Reference,
           user      = User,
           circle    = Circle};
doInit(Parent, proxyLogin, Args) ->
    {PeerUser, ProxyPid, User, Circle} = Args,
    eLog:log(debug, ?MODULE, proxyLogin,
             [self(), PeerUser, ProxyPid, User],
             "Proxy ePeer starting", ?LINE),
    Reference = erlang:monitor(process, ProxyPid),
    State     = #state{stateName = clientConnected,
                       parent    = Parent,
                       peerUser  = PeerUser,
                       proxyPid  = ProxyPid,
                       proxyRef  = Reference,
                       user      = User,
                       circle    = Circle},
    sendCast(proxyLoginReply, [User, PeerUser], State),
    State.

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
handle_call({getLock, Uid, Owner}, From,
            State = #state{stateName = authorized}) ->
    spawnAndReturn(?MODULE, sendCall, [getLock, [Uid, Owner], State],
                   From, State),
    {noreply, State};
handle_call({webProxyCall, Message, Timeout}, From,
            State = #state{stateName = authorized,
                           hashPwd   = HashPwd}) ->
    spawn(?MODULE, doWebProxyCall, [Message, HashPwd, Timeout, From, State]),
    {noreply, State};
handle_call({proxyCall, SrcUser, DestUser, Message, Args}, From,
            State = #state{proxyPid = undefined}) ->
    spawnAndReturn(?MODULE, sendCall,
                   [proxyMsg, [call, SrcUser, DestUser, Message, Args], State],
                   From, State),
    {noreply, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

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
handle_cast({peerCall, From, webProxyCall, Args}, State) ->
    State2 = handlePeerCall(webProxyCall, Args, From, State),
    {noreply, State2};
handle_cast({peerCall, From, Message, Args}, State) ->
    eLog:log(debug, ?MODULE, peerCall, [self(), Message],
             "entering", ?LINE),
    case handlePeerCall(Message, Args, From, State) of
        error ->
            eLog:log(debug, ?MODULE, peerCall, [self(), Message],
                     "error", ?LINE),
            {stop, normal, State};
        State2 ->
            {noreply, State2}
    end;
handle_cast({peerCast, Message, Args}, State) ->
    eLog:log(debug, ?MODULE, peerCast, [self(), Message, Args],
             "entering", ?LINE),
    case handlePeerCast(Message, Args, State) of
        error ->
            eLog:log(debug, ?MODULE, peerCast, [self(), Message],
                     "error", ?LINE),
            {stop, normal, State};
        State2 ->
            eLog:log(debug, ?MODULE, peerCast, [self(), Message],
                     "done", ?LINE),
            {noreply, State2}
    end;
handle_cast(doConnect, State = #state{stateName = connectToPeer,
                                      peerUser  = PeerUser,
                                      user      = User,
                                      port      = Port}) ->
    eLog:log(debug, ?MODULE, doConnect, [self(), User],
             "entering", ?LINE),
    MyTimestamp = createTimestamp(),
    Hash        = getCircleHash(MyTimestamp),

    case sendCall(eTodoConnect, [PeerUser, User, Port, MyTimestamp], State) of
        {ok, Hash, Timestamp} when Timestamp /= MyTimestamp ->
            eLog:log(debug, ?MODULE, doConnect, [self()],
                     "login ok", ?LINE),
            %% Valid hash and different timestamp.
            doRegister(),
            {noreply, State#state{stateName = registerWithPeer,
                                  stateData = Timestamp}};
        _Else ->
            eLog:log(debug, ?MODULE, clientDisconnected, [self(), _Else],
                     "login error", ?LINE),
            %% Not a valid reply.
            {stop, normal, State}
    end;
handle_cast(doRegister, State = #state{stateName = registerWithPeer,
                                       stateData = Timestamp,
                                       peerUser  = PeerUser,
                                       user      = User}) ->
    eLog:log(debug, ?MODULE, doRegister, [self(), User],
             "entering", ?LINE),
    Hash   = getCircleHash(Timestamp),
    Peers  = eTodoDB:getConnections(),
    Peers2 = getPeersWithActiveConnection(Peers),

    case sendCall(eTodoRegister, [User, Hash, eTodoUtils:dateTime(), Peers2],
                  State) of
        {ok, DateTime, RemotePeers, SharedTodos, OldConTime} ->
            loggedIn(State),
            checkConflict(SharedTodos, PeerUser, User, OldConTime),
            updateConnection(DateTime, OldConTime, State),
            ePeerCircle:remotePeers(PeerUser, RemotePeers),
            State2 = launchProxyTimers(State),
            {noreply, State2#state{stateName = authorized,
                                   stateData = undefined}};
        _Else ->
            eLog:log(debug, ?MODULE, doRegister, [self(), User],
                     "login error", ?LINE),
            %% Register did not succeed.
            {stop, normal, State}
    end;
handle_cast(doProxyLogin, State = #state{stateName = proxyToPeer,
                                         peerUser  = PeerUser,
                                         user      = User}) ->
    sendCast(proxyLogin, [User, PeerUser], State),
    {noreply, State};
handle_cast({proxyCast, SrcUser, DestUser, Message, Args}, State) ->
    sendCast(proxyMsg, [cast, SrcUser, DestUser, Message, Args], State),
    {noreply, State};
handle_cast({sendMsg, User, PeerUsers, MsgType, Text},
            State = #state{stateName = authorized}) ->
    sendCast(sendMsg, [User, PeerUsers, MsgType, Text], State),
    {noreply, State};
handle_cast({todoUpdated, PeerUser, Todo},
            State = #state{stateName = authorized,
                           user      = User}) ->

    sendCast(todoUpdated, [User, PeerUser, Todo], State),
    {noreply, State};
handle_cast(stop, State) ->
    eLog:log(debug, ?MODULE, stop, [self(), State#state.user],
             "received stop", ?LINE),
    {stop, normal, State};
handle_cast({releaseLock, Uid, Owner},
            State = #state{stateName = authorized}) ->
    sendCast(releaseLock, [Uid, Owner], State),
    {noreply, State};
handle_cast(Msg, State) ->
    eLog:log(error, ?MODULE, handle_cast, [self(), Msg, State#state.user],
             "Unknown message received", ?LINE),
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
handle_info({'EXIT', Pid, Reason}, State) ->
    eLog:log(debug, ?MODULE, handle_info, [self(), Pid, Reason, State],
             "Received 'EXIT' - shutting down", ?LINE),
    {stop, normal, State};
handle_info({'DOWN', Reference, process, _Object, _Info},
            State = #state{proxyRef = Reference}) ->
    eLog:log(debug, ?MODULE, handle_info, [self(), State],
             "Received 'DOWN' - shutting down", ?LINE),
    {stop, normal, State#state{proxyRef = undefined}};
handle_info({timeout, _OldRef, sendPing},
            State = #state{stateName = authorized,
                           user      = User,
                           peerUser  = PeerUser}) ->
    sendCast(ping, [User, PeerUser], State),
    {noreply, startTimer(sendTimer, State)};
handle_info({timeout, _OldRef, pingMissing}, State) ->
    eLog:log(debug, ?MODULE, handle_info, [self(), State],
             "Haven't received ping - shutting down", ?LINE),
    {stop, normal, State};
handle_info(Info, State) ->
    eLog:log(debug, ?MODULE, handle_info, [self(), Info],
             "Unknown message received", ?LINE),
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
terminate(_Reason, State) ->
    eLog:log(debug, ?MODULE, terminate, [self(), _Reason],
             "shutting down", ?LINE),
    case State#state.proxyRef of
        undefined -> ok;
        Reference -> erlang:demonitor(Reference)
    end,
    case State#state.ePortPid of
        EPortPid when is_pid(EPortPid) ->
            ePort:stop(EPortPid);
        _ ->
            ok
    end,
    cancelTimer(sendTimer, State),
    cancelTimer(recvTimer, State),
    loggedOut(State),
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
%%% BEGIN: Peer to peer functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle call from a another peer.
%%
%% @spec handlePeerCall(Message, Args, State) -> {Reply, NewState} | error
%% @end
%%--------------------------------------------------------------------
handlePeerCall(eTodoConnect,
               [User, PeerUser, PeerPort, Timestamp], From, State) ->
    case eTodoConnect(User, PeerUser, PeerPort, Timestamp, State) of
        {Reply, State2} ->
            gen_server:reply(From, Reply),
            State2;
        error ->
            gen_server:reply(From, error),
            error
    end;
handlePeerCall(eTodoRegister, [PeerUser, Hash, DateTime, Peers], From, State) ->
    case eTodoRegister(PeerUser, Hash, DateTime, Peers, State) of
        {Reply, State2} ->
            gen_server:reply(From, Reply),
            State2;
        error ->
            gen_server:reply(From, error),
            error
    end;
handlePeerCall(Message, Args, _From, State = #state{stateName = StateName})
  when (StateName /= authorized) ->
    eLog:log(error, ?MODULE, handlePeerCall,
             [self(), Message, Args, State#state.user],
             "Unathorized message received.", ?LINE),
    error;
handlePeerCall(proxySend, [SrcUser, DestUser, Message, Args],
               From, State = #state{user = User})
  when (DestUser /= User) ->
    %% Message reached intermediate host.
    spawnAndReturn(?MODULE, sendCall,
                   [proxyMsg, [call, SrcUser, DestUser, Message, Args], State],
                   From, State);
handlePeerCall(proxyMsg, [SrcUser, DestUser, Message, Args],
               From, State = #state{user = User})
  when (DestUser /= User) ->
    %% Message reached intermediate host.
    spawnAndReturn(ePeerServer, proxyCall,
                   [DestUser, proxySend, [SrcUser, DestUser, Message, Args]],
                   From, State);
handlePeerCall(proxyMsg, [SrcUser, DestUser, Message, Args],
               From, State = #state{user = User})
  when (DestUser == User) ->
    %% Message reached final host, but not the right ePeer.
    spawnAndReturn(ePeerServer, proxyCall, [SrcUser, Message, Args],
                   From, State);
handlePeerCall(getLock, [Uid, Owner], From, State) ->
    spawnAndReturn(ePeerLock, getLock, [Uid, Owner], From, State);
handlePeerCall(webProxyCall, [Message, HashPwd, Timeout],
               From, State = #state{hashPwd = HashPwd}) ->
    spawn(?MODULE, doWebProxyCall, [From, Message, Timeout]),
    State;
handlePeerCall(webProxyCall, [Message, HashPwd, Timeout], From, State) ->
    User     = State#state.user,
    UserCfg  = eTodoDB:readUserCfg(User),
    Password = default(UserCfg#userCfg.webPassword, ""),
    HashPwd2 = crypto:hash(sha, Password),

    case HashPwd2 of
        HashPwd ->
            spawn(?MODULE, doWebProxyCall, [From, Message, Timeout]);
        _ ->
            case tokenOK(Message, User, Password) of
                true ->
                    spawn(?MODULE, doWebProxyCall, [From, Message, Timeout]);
                false ->
                    eLog:log(error, ?MODULE, webProxyCall, [self(), Message],
                             "Unauthorized.", ?LINE),
                    gen_server:reply(From, "Unauthorized")
            end
    end,
    State#state{hashPwd = HashPwd2};
handlePeerCall(Message, Args, From, _State) ->
    eLog:log(error, ?MODULE, handlePeerCall, [self(), Message, Args],
             "Unknown message received.", ?LINE),
    gen_server:reply(From, errorUnknownMsg),
    error.

spawnAndReturn(Module, Function, Args, From, State) ->
    spawn(?MODULE, doSpawnAndReturn, [Module, Function, Args, From]),
    State.

doSpawnAndReturn(Module, Function, Args, From) ->
    Reply = apply(Module, Function, Args),
    gen_server:reply(From, Reply).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle cast from a another peer.
%%
%% @spec handlePeerCast(Message, Args, State) -> NewState | error
%% @end
%%--------------------------------------------------------------------
handlePeerCast(proxyLoginReply, [PeerUser, User],
               State = #state{stateName = proxyToPeer,
                              peerUser  = PeerUser,
                              user      = User}) ->
    doConnect(),
    State#state{stateName = connectToPeer};
handlePeerCast(Message, Args, State = #state{stateName = StateName})
  when StateName /= authorized ->
    eLog:log(error, ?MODULE, handlePeerCast,
             [self(), Message, Args, State#state.user],
             "Unathorized message received.", ?LINE),
    error;
handlePeerCast(proxySend, [SrcUser, DestUser, Message, Args],
               State = #state{user = User})
  when (DestUser /= User) ->
    %% Message reached intermediate host.
    sendCast(proxyMsg, [cast, SrcUser, DestUser, Message, Args], State),
    State;
handlePeerCast(proxyMsg, [SrcUser, DestUser, Message, Args],
               State = #state{user = User})
  when (DestUser /= User) ->
    %% Message reached intermediate host.
    ePeerServer:proxyCast(DestUser, proxySend,
                          [SrcUser, DestUser, Message, Args]),
    State;
handlePeerCast(proxyMsg, [SrcUser, DestUser, proxyLogin, [SrcUser, DestUser]],
               State = #state{peerUser = PeerUser,
                              user     = User})
  when (DestUser == User) ->
    ePeerServer:proxyLogin(SrcUser, PeerUser, User),
    State;
handlePeerCast(proxyMsg, [SrcUser, DestUser, Message, Args],
               State = #state{user = User})
  when (DestUser == User) ->
    %% Message reached final host, but not the right ePeer.
    ePeerServer:proxyCast(SrcUser, Message, Args),
    State;
handlePeerCast(sendMsg, Args, State) ->
    sendMsg(Args, State);
handlePeerCast(todoUpdated, [User, PeerUser, Todo], State) ->
    ePeerEM:todoUpdated(User, [PeerUser], Todo),
    State;
handlePeerCast(releaseLock, [Uid, Owner], State) ->
    ePeerLock:releaseLock(Uid, Owner),
    State;
handlePeerCast(ping, [PeerUser, User], State = #state{user     = User,
                                                      peerUser = PeerUser}) ->
    %% Note that pings are only sent between proxy peers.
    %% Ping received - start new recv timer.
    eLog:log(debug, ?MODULE, ping, [self(), PeerUser, User],
             "Received ping.", ?LINE),
    startTimer(recvTimer, State);
handlePeerCast(Message, Args, _State) ->
    eLog:log(error, ?MODULE, handlePeerCast, [self(), Message, Args],
             "Unknown message received.", ?LINE),
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle connect from a peer.
%%
%% @spec eTodoConnect(Args, State) -> {Reply, NewState} | error
%% @end
%%--------------------------------------------------------------------
eTodoConnect(User, PeerUser, PeerPort, Timestamp,
             State = #state{stateName = clientConnected,
                            user      = User}) ->
    case ePeerCircle:isActivePeer(PeerUser) of
        true ->
            eLog:log(error, ?MODULE, eTodoConnect,
                     [self(), User, PeerUser, PeerPort, Timestamp],
                     "User already connected.", ?LINE),
            error;
        false ->
            Hash        = getCircleHash(Timestamp),
            MyTimestamp = createTimestamp(Timestamp),
            Reply       = {ok, Hash, MyTimestamp},
            State2      = State#state{stateName = connected,
                                      stateData = MyTimestamp,
                                      peerUser  = PeerUser,
                                      peerPort  = PeerPort},
            {Reply, State2}
    end;
eTodoConnect(User, PeerUser, PeerPort, Timestamp, _State) ->
    %% Message received in wrong state and is invalid - shut down.
    eLog:log(error, ?MODULE, eTodoConnect,
             [self(), User, PeerUser, PeerPort, Timestamp],
             "Unknown message received.", ?LINE),
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle register from a peeer.
%%
%% @spec eTodoRegister(Args, State) -> {Reply, NewState} | error
%% @end
%%--------------------------------------------------------------------
eTodoRegister(PeerUser, Hash, DateTime, RemotePeers,
              State = #state{stateName = connected,
                             peerUser  = PeerUser,
                             stateData = MyTimestamp,
                             user      = User}) ->
    case getCircleHash(MyTimestamp) of
        Hash ->
            %% Received hash and calculated hash match -> OK.
            loggedIn(State),
            OldConCfg  = default(eTodoDB:getConnection(PeerUser), #conCfg{}),
            OldConTime = OldConCfg#conCfg.updateTime,
            updateConnection(DateTime, OldConTime, State),
            State2 = launchProxyTimers(State),
            ePeerServer:updatePeerInfo(self(), PeerUser),
            ePeerCircle:remotePeers(PeerUser, RemotePeers),
            LocalPeers  = eTodoDB:getConnections(),
            SharedTodos = eTodoDB:getTodosSharedWith(PeerUser, User),
            Reply = {ok, eTodoUtils:dateTime(),
                     LocalPeers, SharedTodos, OldConTime},
            {Reply, State2#state{stateName = authorized,
                                 stateData = undefined}};
        _Else ->
            %% Hash mismatch - error.
            eLog:log(debug, ?MODULE, eTodoRegister,
                     [self(), _Else, PeerUser],
                     "Invalid hash from peer.", ?LINE),
            error
    end;
eTodoRegister(PeerUser, _Hash, _DateTime, _Peers, #state{user = User}) ->
    %% Message received in wrong state and is invalid - shut down.
    eLog:log(error, ?MODULE, eTodoRegister,
             [self(), User, PeerUser],
             "Unknown message received.", ?LINE),
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle chat message from peer
%%
%% @spec sendMsg(Args, State) -> {Reply, NewState} | error
%% @end
%%--------------------------------------------------------------------
sendMsg([User, PeerUsers, MsgType, Text], State) ->
    eLog:log(debug, ?MODULE, sendMsg, [self(), User, PeerUsers, MsgType, Text],
             "calling ePeerEM", ?LINE),
    ePeerEM:sendMsg(User, PeerUsers, MsgType, Text),
    State.

%%%===================================================================
%%% END: Peer to peer functions
%%%===================================================================

%%%===================================================================
%%% BEGIN: Internal functions
%%%===================================================================
getPeersWithActiveConnection(Peers) ->
    RemoveNonActive = fun (Peer) ->
                              ePeerCircle:isActivePeer(Peer)
                      end,
    lists:map(RemoveNonActive, Peers).

loggedIn(#state{user     = User,
                peerUser = PeerUser,
                circle   = Circle}) ->
    ePeerEM:add_handler({ePeerEH, PeerUser}, {self(), User, PeerUser}),
    ePeerEM:loggedIn(PeerUser),
    ePeerCircle:peerStatus(PeerUser, active, Circle),
    ok.

loggedOut(#state{peerUser = PeerUser,
                 circle   = Circle}) ->
    ePeerEM:del_handler({ePeerEH, PeerUser}, shutdown),
    case PeerUser of
        undefined -> ok;
        _         -> ePeerEM:loggedOut(PeerUser)
    end,
    ePeerCircle:peerStatus(PeerUser, inactive, Circle),
    ok.

updateConnection(_DateTime, configured, _State) ->
    ok;
updateConnection(DateTime, _OldDateTime, #state{peerUser   = PeerUser,
                                                peerHost   = PeerHost,
                                                peerPort   = PeerPort,
                                                user       = User}) ->
    ConCfg = #conCfg{userName   = PeerUser,
                     host       = PeerHost,
                     port       = PeerPort,
                     owner      = User,
                     distance   = 1,
                     updateTime = DateTime},
    eTodoDB:updateConnection(ConCfg).

createTimestamp() ->
    ePeerCircle:createTimestamp().

createTimestamp(Timestamp) ->
    case ePeerCircle:createTimestamp() of
        Timestamp ->
            %% Not allowed to use same Timestamp.
            createTimestamp(Timestamp);
        MyTimestamp ->
            MyTimestamp
    end.

getCircleHash(Timestamp) ->
    ePeerCircle:getHash(Timestamp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send get lock to other peer.
%%
%% @spec doWebProxyCall(Message, HashPwd, Timeout, From, State) -> ok
%% @end
%%--------------------------------------------------------------------
doWebProxyCall(Message, HashPwd, Timeout, From, State) ->
    Reply = sendCall(webProxyCall,
                     [Message, HashPwd, Timeout],
                     Timeout, State),
    gen_server:reply(From, Reply).

doWebProxyCall(From, Message, Timeout) ->
    Reply = eWeb:webProxyCall(Message, Timeout),
    gen_server:reply(From, Reply).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check for task edit conflicts
%%
%% @spec checkConflict(SharedTodos, PeerUser, User, OldConTime) -> ok
%% @end
%%--------------------------------------------------------------------
checkConflict(SharedTodos, PeerUser, User, OldRemoteConTime) ->
    OldLocalConCfg   = default(eTodoDB:getConnection(PeerUser), #conCfg{}),
    OldLocalConTime  = OldLocalConCfg#conCfg.updateTime,
    SharedLocalTodos = eTodoDB:getTodosSharedWith(PeerUser, User),
    case (SharedTodos == SharedLocalTodos) of
        true ->
            %% No potential conflict, lists exactly the same.
            ok;
        false ->
            ePeerEM:checkConflict(User, PeerUser,
                                  OldLocalConTime, OldRemoteConTime,
                                  {SharedLocalTodos, SharedTodos})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Timer functions.
%%
%% @spec startTimer(Name, State) -> NewState
%% @end
%% @spec cancelTimer(Name, State) -> NewState
%% @end
%%--------------------------------------------------------------------
launchProxyTimers(State = #state{proxyPid = undefined}) ->
    %% No timers when not a proxy peer.
    State;
launchProxyTimers(State) ->
    State2 = startTimer(sendTimer, State),
    startTimer(recvTimer, State2).

startTimer(sendTimer, State) ->
    State2    = cancelTimer(sendTimer, State),
    Reference = erlang:start_timer(?SendPingDelay, self(), sendPing),
    State2#state{sendPingTimer = Reference};
startTimer(recvTimer, State) ->
    State2    = cancelTimer(recvTimer, State),
    Reference = erlang:start_timer(?RecvPingDelay, self(), pingMissing),
    State2#state{recvPingTimer = Reference}.

cancelTimer(sendTimer, State = #state{sendPingTimer = undefined}) ->
    State;
cancelTimer(sendTimer, State = #state{sendPingTimer = Reference}) ->
    erlang:cancel_timer(Reference),
    State#state{sendPingTimer = undefined};
cancelTimer(recvTimer, State = #state{recvPingTimer = undefined}) ->
    State;
cancelTimer(recvTimer, State = #state{recvPingTimer = Reference}) ->
    erlang:cancel_timer(Reference),
    State#state{recvPingTimer = undefined}.

tokenOK({_Name, _SessionId, _Env, Input}, User, Password) ->
    KeyValueList = httpd:parse_query(Input),
    Token        = http_uri:decode(
                     proplists:get_value("token", KeyValueList, "")),
    Token == base64:encode_to_string(crypto:hash(sha, User ++ "@" ++ Password)).

%%%===================================================================
%%% END: Internal functions
%%%===================================================================
