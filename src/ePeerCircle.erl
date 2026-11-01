%%%-------------------------------------------------------------------
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @author Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2012 by Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%%-------------------------------------------------------------------
-module(ePeerCircle).

-behaviour(gen_server).

-include_lib("eTodo/include/eTodo.hrl").

%% API
-export([start_link/0,
         createTimestamp/0,
         getHash/1,
         getPwd/0,
         connectToCircle/3,
         isActivePeer/1,
         loggedOutFromCircle/1,
         peerStatus/3,
         remotePeers/2
        ]).

%% Local call progress
-export([nextPeer/0,
         addToConQueue/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,

         terminate/2,
         code_change/3]).

-import(eTodoUtils, [default/2, getRootDir/0]).

-define(SERVER,         ?MODULE).
-define(ConnectMaxTime, 10000).
-define(PeerCleanupInterval, 5 * 60 * 1000).
-define(PeerRetentionSeconds, 60 * 24 * 60 * 60).
-define(FirstRetryDelay, 5000).
-define(MaxRetryDelay, 5 * 60 * 1000).

-record(peer, {user,
               circle,
               status,
               retryDelay = ?FirstRetryDelay,
               retryRef}).

-record(state, {user,
                circle,
                password,
                host,
                port,
                listenerPid,
                tsHandle,
                stateName,
                peers       = [],
                conQueue    = [],
                remoteQueue = [],
                loopActive  = false,
                connectData = [],
                proxyCandidates = []
               }).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Create a timestamp
%%
%% @spec createTimeStamp() -> Timestamp
%% @end
%%--------------------------------------------------------------------
createTimestamp() ->
    gen_server:call(?SERVER, createTimestamp).

%%--------------------------------------------------------------------
%% @doc
%% Genereate password hash
%%
%% @spec getHash(Circle, Timestamp) -> Hash
%% @end
%%--------------------------------------------------------------------
getHash(Timestamp) ->
    gen_server:call(?SERVER, {getHash, Timestamp}).

%%--------------------------------------------------------------------
%% @doc
%% Get circle password
%%
%% @spec getPwd() -> Pwd
%% @end
%%--------------------------------------------------------------------
getPwd() ->
    gen_server:call(?SERVER, getPwd).

%%--------------------------------------------------------------------
%% @doc
%% Check if peer is active.
%%
%% @spec isActivePeer(User) -> true | false
%% @end
%%--------------------------------------------------------------------
isActivePeer(User) ->
    gen_server:call(?SERVER, {isActivePeer, User}).

%%--------------------------------------------------------------------
%% @doc
%% Connect to a P2P circle.
%%
%% @spec connectToCircle(User, Circle, Password) -> ok
%% @end
%%--------------------------------------------------------------------
connectToCircle(User, Circle, Password) ->
    gen_server:cast(?SERVER, {connectToCircle, User, Circle, Password}).

%%--------------------------------------------------------------------
%% @doc
%% Logged out from a P2P circle.
%%
%% @spec loggedOutFromCircle(User) -> ok
%% @end
%%--------------------------------------------------------------------
loggedOutFromCircle(User) ->
    gen_server:cast(?SERVER, {loggedOutFromCircle, User}).

%%--------------------------------------------------------------------
%% @doc
%% Update status for a peer.
%%
%% @spec peerStatus(PeerUser, Status, Circle) -> ok
%% @end
%%--------------------------------------------------------------------
peerStatus(PeerUser, Status, Circle) ->
    gen_server:cast(?SERVER, {peerStatus, PeerUser, Status, Circle}).

%%--------------------------------------------------------------------
%% @doc
%% Update status for a peer.
%%
%% @spec remotePeers(PeerUser, PeerList) -> ok
%% @end
%%--------------------------------------------------------------------
remotePeers(PeerUser, PeerList) ->
    gen_server:cast(?SERVER, {remotePeers, PeerUser, PeerList}).

%%--------------------------------------------------------------------
%% @doc
%% Add connections to connection queue.
%%
%% @spec addToConQueue(Owner, Connections) -> ok
%% @end
%%--------------------------------------------------------------------
addToConQueue(Owner, Connections) ->
    gen_server:cast(?SERVER, {addToConQueue, Owner, Connections}).

%%--------------------------------------------------------------------
%% @doc
%% Launch login to next peer.
%%
%% @spec nextPeer() -> ok
%% @end
%%--------------------------------------------------------------------
nextPeer() ->
    gen_server:cast(?SERVER, nextPeer).

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
    process_flag(trap_exit, true),
    erlang:start_timer(?PeerCleanupInterval, self(), cleanupPeers),
    ePeerEM:add_handler(ePeerCircleEH, []),
    {ok, #state{tsHandle  = rand:uniform(100000000),
                stateName = idle}}.

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
handle_call(createTimestamp, _From, State) ->
    {Timestamp, State2} = getNextTimestamp(State),
    {reply, Timestamp, State2};
handle_call({getHash, Timestamp}, _From, State) ->
    {Hash, State2} = getHash(Timestamp, State),
    {reply, Hash, State2};
handle_call(getPwd, _From, State = #state{password = Pwd}) ->
    {reply, Pwd, State};
handle_call({isActivePeer, User}, _From, State) ->
    IsActive = checkIfActive(User, State),
    {reply, IsActive, State};
handle_call(Request, From, State) ->
    eLog:log(error, ?MODULE, handle_call, [Request, From],
             "Unknown message received", ?LINE),
    {reply, {error, unknownRequest}, State}.

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
handle_cast({connectToCircle, User, CircleName, Password}, State) ->
    case launchListener(User) of
        {ok, Port, Pid} ->
            ePeerEM:acceptingIncCon(User, CircleName, Port),
            updateOwnConfig(User, Port),
            announceWebGui(User),
            ePeerServer:connectedToCircle(User, CircleName),
            State0      = cleanupOldConnections(State#state{user = User}),
            Connections = getClearedConnections(),
            ConQueue    = queueAdd(User, Connections, []),
            State2      = conLoop(State0),
            eLog:log(debug, ?MODULE, connectToCircle,
                     [User, CircleName, Password, ConQueue],
                     "circle active", ?LINE),
            {noreply, State2#state{user        = User,
                                   port        = Port,
                                   listenerPid = Pid,
                                   circle      = CircleName,
                                   password    = Password,
                                   conQueue    = ConQueue,
                                   stateName   = directSetup}};
        _Else ->
            {stop, noListener, State}
    end;
handle_cast({loggedOutFromCircle, User}, State = #state{listenerPid = Pid}) ->
    ePeerServer:loggedOutFromCircle(),
    State2 = logoutAllPeers(State),
    State3 = State2#state{circle      = undefined,
                          password    = undefined,
                          conQueue    = [], % Discard any data left.
                          remoteQueue = [], % Discard any data left.
                          loopActive  = false,
                          stateName   = idle,
                          proxyCandidates = [],
                          connectData = [],
                          listenerPid = undefined},
    [erlang:cancel_timer(Ref) || {_PeerUser, Ref} <- State#state.connectData],
    eLog:log(debug, ?MODULE, loggedOutFromCircle, [User, State3],
             "circle cleared", ?LINE),
    ePortListener:stop(Pid),
    {noreply, State3};
handle_cast(nextPeer, State = #state{loopActive = true}) ->
    State2 = nextPeer(State#state{loopActive = false}),
    {noreply, State2};
handle_cast(nextPeer, State) ->
    eLog:log(debug, ?MODULE, nextPeer, [],
             "Dropped - loop not active.", ?LINE),
    {noreply, State};
handle_cast({peerStatus, PeerUser, Status, Circle},
            State = #state{circle = Circle}) ->
    %% Status from peer creation received.
    State2 = handleConnectData(PeerUser, State),
    State3 = savePeerStatus(PeerUser, Status, State2),
    State4 = conLoop(State3),
    {noreply, State4};
handle_cast({peerStatus, PeerUser, Status, OldCircle}, State) ->
    %% Status received from peer launched when a different circle
    %% was active - ignore.
    eLog:log(debug, ?MODULE, peerStatus, [PeerUser, Status, OldCircle, State],
             "Status from 'old' ePeer ignored.", ?LINE),
    {noreply, State};
handle_cast({remotePeers, PeerUser, PeerList},
            State = #state{conQueue    = ConQueue,
                           remoteQueue = RemoteQueue}) when is_list(PeerList) ->
    RemoteQueue2 = lists:delete(PeerUser, RemoteQueue),
    ConQueue2    = queueAdd(PeerUser, PeerList, ConQueue),
    State2       = conLoop(State),
    {noreply, State2#state{conQueue    = ConQueue2,
                           remoteQueue = RemoteQueue2}};
handle_cast({addToConQueue, Owner, Connections},
            State= #state{conQueue = ConQueue}) ->
    ConQueue2 = queueAdd(Owner, Connections, ConQueue),
    State2    = conLoop(State),
    {noreply, State2#state{conQueue = ConQueue2}};

handle_cast(Message, State) ->
    eLog:log(error, ?MODULE, handle_cast, [Message, State],
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
handle_info({timeout, Ref, connectTimeout},
            State = #state{connectData = ConData}) ->
    ConData3 =
        case lists:keytake(Ref, 2, ConData) of
            false ->
                eLog:log(debug, ?MODULE, handle_info, [Ref],
                         "Timer already removed.", ?LINE),
                ConData;
            {value, {PeerUser, Ref}, ConData2} ->
                %% Expected message from launching peer not received.
                eLog:log(debug, ?MODULE, handle_info, [PeerUser],
                         "Peer not properly launched", ?LINE),
                closeConnectionToPeer(PeerUser),
                ConData2
        end,
    State2 = conLoop(State),
    {noreply, State2#state{connectData = ConData3}};
handle_info({timeout, Ref, {retryPeer, PeerUser}}, State) ->
    case expireRetry(PeerUser, Ref, State) of
        {retry, State2} ->
            case eTodoDB:getConnection(PeerUser) of
                undefined ->
                    Peers = lists:keydelete(PeerUser, #peer.user,
                                            State2#state.peers),
                    {noreply, State2#state{peers = Peers}};
                Connection ->
                    {noreply, queueRetryConnection(Connection, State2)}
            end;
        {ignore, State2} ->
            {noreply, State2}
    end;
handle_info({timeout, _, cleanupPeers}, State) ->
    State2 = cleanupOldConnections(State),
    State3 = queueUnscheduledConnections(State2),
    erlang:start_timer(?PeerCleanupInterval, self(), cleanupPeers),
    {noreply, State3};
handle_info(Info, State) ->
    eLog:log(error, ?MODULE, handle_info, [Info, State#state.connectData],
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
    [cancelPeerRetry(Peer) || Peer <- State#state.peers],
    [erlang:cancel_timer(Ref) || {_PeerUser, Ref} <- State#state.connectData],
    ePeerEM:del_handler(ePeerCircleEH, shutdown),
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

%% Tells the user where the web GUI can be reached.
announceWebGui(User) ->
    case application:get_env(eTodo, mistWebEnabled, false) of
        true ->
            Port = integer_to_list(application:get_env(eTodo, mistWebPort,
                                                       9003)),
            Host = case eTodoDB:getConnection(User) of
                       #conCfg{host = H} when is_list(H), H =/= "" -> H;
                       _ -> "localhost"
                   end,
            ePeerEM:sendMsg(system, [User], systemEntry,
                            "<https://" ++ Host ++ ":" ++ Port ++ "/> opens "
                            "the web GUI in a browser."),
            case application:get_env(eTodo, mistWebLinkHost) of
                {ok, Link} when is_list(Link) ->
                    ePeerEM:sendMsg(system, [User], systemEntry,
                                    "<https://" ++ Link ++ "/> opens the web "
                                    "GUI through the relay.");
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start listening for remote connections.
%%
%% @spec launchListener(User) -> ok
%% @end
%%--------------------------------------------------------------------
launchListener(User) ->
    #userCfg{conPort = LowPort} = eTodoDB:readUserCfg(User),
    LowPort2 = default(LowPort, 19000),
    launchListener(LowPort2, LowPort2 + 10).

launchListener(Port, MaxPort) when Port =< MaxPort ->
    Root      = getRootDir(),
    _         = eTodoCerts:ensure(Root),
    FullChain = filename:join([Root, "server_cert.pem.fullchain.pem"]),
    CertFile  = case filelib:is_file(FullChain) of
                    true  -> FullChain;
                    false -> filename:join([Root, "server_cert.pem"])
                end,
    ServerKey = filename:join([Root, "server_key.pem"]),
    case catch ePortListener:start_link(ePeerProtocol, Port, all,
                                        [{certfile, CertFile},
                                         {keyfile, ServerKey}]) of
        {ok, Pid} ->
            {ok, Port, Pid};
        _ ->
            launchListener(Port + 1, MaxPort)
    end;
launchListener(_Port, _MaxPort) ->
    error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% TBD
%%
%% @spec nextPeer(State) -> NewState
%% @end
%%--------------------------------------------------------------------
nextPeer(State = #state{stateName   = directSetup,
                        conQueue    = [],
                        remoteQueue = [],
                        proxyCandidates = Candidates}) ->
    %% No more connections to try to connect to, either direct or expected.
    %% Try launching proxy peers.
    %% Only fall back for this round's direct attempts, not peers in backoff.
    Connections = [Connection ||
                      Connection = #conCfg{userName = User} <-
                          eTodoDB:getConnections(),
                      lists:member(User, Candidates)],
    ConQueue    = queueAdd(proxy, Connections, []),
    eLog:log(debug, ?MODULE, nextPeer, [ConQueue],
             "All queues cleared, try to proxy", ?LINE),
    State2 = conLoop(State),
    State2#state{stateName = proxySetup,
                 proxyCandidates = [],
                 conQueue  = ConQueue};
nextPeer(State = #state{conQueue    = [],
                        remoteQueue = []}) ->
    %% No more connections to try to connect to, either direct or expected.
    %% Not time to try proxy connections (stateName = proxySetup | idle).
    %% No more work.
    eLog:log(debug, ?MODULE, nextPeer, [],
             "All queues cleared - go idle", ?LINE),
    State#state{stateName  = idle,
                loopActive = false};
nextPeer(State = #state{conQueue    = []}) ->
    %% No more connections in queue but still waiting for remote peers
    %% from at least one peer - no change".
    eLog:log(debug, ?MODULE, nextPeer, [],
             "Connection queue cleared - awaiting remote peers", ?LINE),
    State#state{loopActive = false};
nextPeer(State = #state{conQueue = [{Owner, ConCfg} | ConQueue]}) ->
    {Result, State2} = nextPeer(Owner, ConCfg,
                                State#state{conQueue = ConQueue}),
    case Result of
        next -> conLoop(State2);
        _    -> State2#state{loopActive = false}
    end.


nextPeer(proxy, #conCfg{userName = User}, State = #state{user = User}) ->
    %% Don't connect to yourself.
    {next, State};
nextPeer(proxy, ConCfg = #conCfg{owner = undefined},
         State = #state{user = User}) ->
    %% No known peer has access to this peer at the moment.
    %% No one to use as proxy.
    eLog:log(debug, ?MODULE, nextPeer, [ConCfg, User],
             "No one to use as proxy.", ?LINE),
    {next, State};
nextPeer(proxy, #conCfg{owner = User}, State = #state{user = User}) ->
    %% Already successfully connected to this peer.
    {next, State};
nextPeer(proxy, #conCfg{userName = PeerUser,
                        owner    = ProxyUser}, State = #state{user = User}) ->
    %% A peer someone else can reach but not we. Use them as proxy.
    case {getPeerStatus(PeerUser, State), getPeerStatus(ProxyUser, State)} of
        %% Peer we try to connect to is unknown, and proxy used is active.
        {undefined, #peer{status = active}} ->
            ePeerServer:proxyToPeer(PeerUser, ProxyUser, User),
            State2 = setConnectData(PeerUser, State),
            State3 = savePeerStatus(PeerUser, connecting, State2),
            {connecting, State3};
        %% Peer we try to connect to is inactive, and proxy used is active
        {#peer{status = inactive}, #peer{status = active}} ->
            ePeerServer:proxyToPeer(PeerUser, ProxyUser, User),
            State2 = setConnectData(PeerUser, State),
            State3 = savePeerStatus(PeerUser, connecting, State2),
            {connecting, State3};
        _ ->
            {next, State}
    end;
nextPeer(User, #conCfg{userName = User}, State = #state{user = User}) ->
    %% Don't connect to yourself.
    {next, State};
nextPeer(User, ConCfg, State = #state{user = User}) ->
    %% This is one of our own connections. Try to connect using it.
    eLog:log(debug, ?MODULE, nextPeer,
             [User, ConCfg], "testing local ConCfg", ?LINE),
    connectToPeer(ConCfg, State);
nextPeer(_PeerUser, #conCfg{userName = User,
                            host     = Host}, State = #state{user = User}) ->
    %% This is data a remote peer has about us - use some data.
    MyConCfg  = getConnection(User),
    MyConCfg2 = MyConCfg#conCfg{host       = default(MyConCfg#conCfg.host, Host),
                                updateTime = eTodoUtils:dateTime()},

    if MyConCfg#conCfg.updateTime == configured ->
            ok;
       true ->
            eTodoDB:updateConnection(MyConCfg2)
    end,
    {next, State};
nextPeer(PeerUser, #conCfg{userName = PeerUser,
                           port     = Port,
                           email    = Email}, State) ->
    %% This is data a peer we (got) connected to has about itself.
    %% No need to connect again - but use some of it's data.
    PeerConCfg   = getConnection(PeerUser),
    case PeerConCfg#conCfg.updateTime of
        configured ->
            ok;
        _ ->
            eTodoDB:updateConnection(PeerConCfg#conCfg{port = Port,
                                                       email = Email})
    end,
    {next, State};
nextPeer(PeerUser, ConCfg = #conCfg{userName = RemoteUser},
         State = #state{user = User}) ->
    %% A remote user known by a peer we connected to.
    %% Check our status and take action.
    case getPeerStatus(RemoteUser, State) of
        undefined ->
            eLog:log(debug, ?MODULE, nextPeer,
                     [ConCfg, PeerUser, User, State],
                     "previously unknown user", ?LINE),
            evalRemoteConnect(ConCfg, PeerUser, State);
        #peer{status = active} ->
            eLog:log(debug, ?MODULE, nextPeer,
                     [ConCfg, PeerUser, User, State],
                     "previously active peer", ?LINE),
            {next, State};
        #peer{status = connecting} ->
            eLog:log(debug, ?MODULE, nextPeer,
                     [ConCfg, PeerUser, User, State],
                     "peer still in status connecting "
                     "put peer last in conQueue and try "
                     "again later.", ?LINE),
            %% Wait ten seconds to avoid reconnecting directly.
            timer:apply_after(10000, ?MODULE, addToConQueue,
                              [PeerUser, [ConCfg]]),
            {next, State};
        _Peer ->
            eLog:log(debug, ?MODULE, nextPeer,
                     [ConCfg, PeerUser, User, State],
                     "inactive peer", ?LINE),
            evalRemoteConnect(ConCfg, PeerUser, State)
    end;
%% Convert version 1 conCfg records to new format.
nextPeer(PeerUser,
         {conCfg, User, Host, Port, Owner, Distance, UpdateTime}, State) ->
    nextPeer(PeerUser, #conCfg{userName   = User,
                               host       = Host,
                               port       = Port,
                               owner      = Owner,
                               distance   = Distance,
                               updateTime = UpdateTime}, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get new timestamp for circle.
%%
%% @spec getNextTimestamp(State) -> {Timestamp, NewState}
%% @end
%%--------------------------------------------------------------------
getNextTimestamp(State = #state{tsHandle = OldTimestamp}) ->
    Timestamp = OldTimestamp + 1,
    {Timestamp, State#state{tsHandle = Timestamp}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get password hash for circle.
%%
%% @spec getHash(Circle, Timestamp, State) -> {Hash, NewState}
%% @end
%%--------------------------------------------------------------------
getHash(Timestamp, State = #state{password = undefined}) ->
    Hash = crypto:hash(sha, integer_to_list(Timestamp)),
    {Hash, State};
getHash(Timestamp, State = #state{password = Password}) ->
    Hash = crypto:hash(sha, integer_to_list(Timestamp) ++ Password),
    {Hash, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get local status for peer.
%%
%% @spec getPeerStatus(PeerUser, State) -> Peer | undefined
%% @end
%%--------------------------------------------------------------------
getPeerStatus(PeerUser, #state{peers = Peers}) ->
    case lists:keysearch(PeerUser, #peer.user, Peers) of
        {value, Peer} -> Peer;
        _             -> undefined
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save local status for peer.
%%
%% @spec savePeerStatus(PeerUser, Status, State) -> NewState
%% @end
%%--------------------------------------------------------------------
savePeerStatus(PeerUser, Status,
               State = #state{peers       = Peers,
                              remoteQueue = RemoteQueue}) ->
    Peer = case lists:keysearch(PeerUser, #peer.user, Peers) of
               {value, ExistingPeer} -> ExistingPeer;
               _ -> #peer{user = PeerUser}
           end,
    Peer2 = updatePeerRetry(Status, Peer, State),
    Peers2 = lists:keystore(PeerUser, #peer.user, Peers, Peer2),
    RemoteQueue2 = case Status of
                       active -> [PeerUser | lists:delete(PeerUser, RemoteQueue)];
                       inactive -> [User || User <- RemoteQueue,
                                           User =/= PeerUser];
                       _ -> RemoteQueue
                   end,
    State#state{peers       = Peers2,
                remoteQueue = RemoteQueue2}.

updatePeerRetry(active, Peer, _State) ->
    Peer2 = cancelPeerRetry(Peer),
    Peer2#peer{status = active, retryDelay = ?FirstRetryDelay};
updatePeerRetry(connecting, Peer, _State) ->
    (cancelPeerRetry(Peer))#peer{status = connecting};
updatePeerRetry(inactive, Peer, #state{circle = Circle, user = User})
  when Circle =/= undefined, Peer#peer.user =/= undefined,
       Peer#peer.user =/= User ->
    Peer2 = case Peer#peer.status of
                active -> Peer#peer{retryDelay = ?FirstRetryDelay};
                _ -> Peer
            end,
    case Peer2#peer.retryRef of
        undefined ->
            Ref = erlang:start_timer(Peer2#peer.retryDelay, self(),
                                     {retryPeer, Peer2#peer.user}),
            Peer2#peer{status = inactive, retryRef = Ref};
        _ ->
            Peer2#peer{status = inactive}
    end;
updatePeerRetry(Status, Peer, _State) ->
    (cancelPeerRetry(Peer))#peer{status = Status}.

cancelPeerRetry(Peer = #peer{retryRef = undefined}) ->
    Peer;
cancelPeerRetry(Peer = #peer{retryRef = Ref}) ->
    erlang:cancel_timer(Ref),
    Peer#peer{retryRef = undefined}.

expireRetry(PeerUser, Ref, State = #state{circle = Circle, peers = Peers})
  when Circle =/= undefined ->
    case getPeerStatus(PeerUser, State) of
        Peer = #peer{status = inactive, retryRef = Ref, retryDelay = Delay} ->
            Peer2 = Peer#peer{retryRef = undefined,
                              retryDelay = min(Delay * 2, ?MaxRetryDelay)},
            {retry, State#state{peers = lists:keystore(
                                         PeerUser, #peer.user, Peers, Peer2)}};
        _ ->
            {ignore, State}
    end;
expireRetry(_PeerUser, _Ref, State) ->
    {ignore, State}.

queueRetryConnection(Connection, State = #state{user = User,
                                                conQueue = ConQueue}) ->
    conLoop(State#state{stateName = directSetup,
                         conQueue = [{User, Connection} | ConQueue]}).

queueUnscheduledConnections(State = #state{circle = undefined}) ->
    State;
queueUnscheduledConnections(State = #state{user = User, conQueue = ConQueue}) ->
    Connections = [Connection ||
                      Connection = #conCfg{userName = PeerUser} <-
                          eTodoDB:getConnections(),
                      PeerUser =/= User,
                      readyForDirectConnection(PeerUser, State)],
    case Connections of
        [] -> State;
        _ ->
            conLoop(State#state{stateName = directSetup,
                                 conQueue = queueAdd(User, Connections, ConQueue)})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set all peer information to logged out status.
%%
%% @spec logoutAllPeers(State) -> NewState
%% @end
%%--------------------------------------------------------------------
logoutAllPeers(State) ->
    logoutAllPeers(State#state.peers, [], State).

logoutAllPeers([], Acc, State) ->
    State#state{peers = Acc};
logoutAllPeers([Peer = #peer{user = User} | Rest], Acc, State) ->
    %% Set connection data to indicate no active connection exist.
    ConCfg = getConnection(User),
    saveConnection(ConCfg#conCfg{owner    = undefined,
                                 distance = undefined}),
    Peer2 = cancelPeerRetry(Peer),
    logoutAllPeers(Rest, [Peer2#peer{circle = undefined,
                                     status = inactive,
                                     retryDelay = ?FirstRetryDelay} | Acc], State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Clear all connection, to be used when logging into a new circle.
%%
%% @spec getClearedConnections() -> ClearedConnections
%% @end
%%--------------------------------------------------------------------
getClearedConnections() ->
    Connections = eTodoDB:getConnections(),
    clearConnections(Connections, []).

clearConnections([], ClearedConnections) ->
    lists:reverse(ClearedConnections);
clearConnections([ConCfg|Rest], SoFar) ->
    ClearedConCfg = ConCfg#conCfg{owner    = undefined,
                                  distance = undefined},
    saveConnection(ClearedConCfg),
    clearConnections(Rest, [ClearedConCfg|SoFar]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update own connection config.
%%
%% @spec updateOwnConfig(User, Port) -> NewState
%% @end
%%--------------------------------------------------------------------
updateOwnConfig(User, Port) ->
    ConCfg  = getConnection(User),
    ConCfg2 = ConCfg#conCfg{userName   = User,
                            port       = Port,
                            updateTime = eTodoUtils:dateTime()},
    if ConCfg#conCfg.updateTime == configured ->
            ok;
       true ->
            eTodoDB:updateConnection(ConCfg2)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save connection info (if "closer")
%%
%% @spec saveRemoteConnection() -> TBD
%% @end
%%--------------------------------------------------------------------
evalRemoteConnect(#conCfg{host = undefined},     _PeerUser, State) ->
    {next, State};
evalRemoteConnect(#conCfg{host = ""},            _PeerUser, State) ->
    {next, State};
evalRemoteConnect(#conCfg{port = undefined},     _PeerUser, State) ->
    {next, State};
evalRemoteConnect(#conCfg{port = ""},            _PeerUser, State) ->
    {next, State};
evalRemoteConnect(#conCfg{distance = undefined}, _PeerUser, State) ->
    {next, State};
evalRemoteConnect(ConCfg, PeerUser, State) ->
    Distance     = ConCfg#conCfg.distance,
    RemoteUser   = ConCfg#conCfg.userName,
    LocalConCfg  = getConnection(RemoteUser),
    RemoteConCfg =
        case LocalConCfg#conCfg.updateTime of
            configured ->
                LocalConCfg#conCfg{owner = PeerUser,
                                   distance = Distance + 1};
            UpdateTime ->
                ConCfg#conCfg{owner      = PeerUser,
                              distance   = Distance + 1,
                              updateTime = UpdateTime}
        end,
    markPeerSeen(RemoteUser),
    saveConnection(RemoteConCfg),
    connectToPeer(ConCfg, State). %% Yes, use ConCfg.

connectToPeer(#conCfg{host     = undefined}, State) ->
    {next, State};
connectToPeer(#conCfg{host     = ""}, State) ->
    {next, State};
connectToPeer(#conCfg{port     = undefined}, State) ->
    {next, State};
connectToPeer(#conCfg{port     = ""}, State) ->
    {next, State};
connectToPeer(#conCfg{userName = PeerUser,
                      host     = PeerHost,
                      port     = PeerPort},
              State = #state{user = User,
                             port = Port}) ->
    case readyForDirectConnection(PeerUser, State) of
        true ->
            launchDirectConnection(PeerUser, PeerHost, PeerPort, User, Port, State);
        false ->
            {next, State}
    end.

readyForDirectConnection(PeerUser, State) ->
    case getPeerStatus(PeerUser, State) of
        undefined -> true;
        #peer{status = inactive, retryRef = undefined} -> true;
        _ -> false
    end.

launchDirectConnection(PeerUser, PeerHost, PeerPort, User, Port,
                       State = #state{proxyCandidates = Candidates}) ->
    connectToPeer(PeerUser, PeerHost, PeerPort, User, Port),
    State2 = setConnectData(PeerUser, State),
    State3 = savePeerStatus(PeerUser, connecting, State2),
    {connecting, State3#state{proxyCandidates = lists:usort(
                                                [PeerUser | Candidates])}}.

connectToPeer(PeerUser, PeerHost, PeerPort, User, Port) ->
    ePeerServer:connectToPeer(PeerUser, PeerHost, PeerPort, User, Port).

conLoop(State = #state{loopActive = true}) ->
    State;
conLoop(State) ->
    nextPeer(),
    State#state{loopActive = true}.

notToOld(undefined) ->
    true; %% We do not know how old this connection is, updateConnection
          %% will set an update time to now.
notToOld(configured) ->
    true; %% This is a configured connection, keep it.
notToOld(UpdateTime) ->
    Diff = calendar:datetime_to_gregorian_seconds(eTodoUtils:dateTime()) -
        calendar:datetime_to_gregorian_seconds(UpdateTime),
    Diff < (3600 * 24 * 60).  %% Not older than 60 days.

cleanupOldConnections(State = #state{peers = Peers}) ->
    Now = calendar:datetime_to_gregorian_seconds(eTodoUtils:dateTime()),
    Cutoff = calendar:gregorian_seconds_to_datetime(
               Now - ?PeerRetentionSeconds),
    ProtectedUsers = [State#state.user |
                      [User || #peer{user = User, status = Status} <- Peers,
                               Status =:= active orelse Status =:= connecting]],
    case eTodoDB:removeStaleConnections(Cutoff, ProtectedUsers) of
        {atomic, RemovedUsers} ->
            Removed = sets:from_list(RemovedUsers),
            [cancelPeerRetry(Peer) || Peer = #peer{user = User} <- Peers,
                                      sets:is_element(User, Removed)],
            State#state{peers = [Peer || Peer = #peer{user = User} <- Peers,
                                         not sets:is_element(User, Removed)]};
        {aborted, Reason} ->
            eLog:log(error, ?MODULE, cleanupOldConnections,
                     [Cutoff, Reason],
                     "Failed to clean up stale peer connections", ?LINE),
            State
    end.

markPeerSeen(undefined) ->
    ok;
markPeerSeen(User) ->
    case eTodoDB:updatePeerLastSeen(User, eTodoUtils:dateTime()) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            eLog:log(error, ?MODULE, markPeerSeen, [User, Reason],
                     "Failed to update peer last-seen time", ?LINE)
    end.

saveConnection(ConCfg) ->
    case notToOld(ConCfg#conCfg.updateTime) of
        true ->
            eTodoDB:updateConnection(ConCfg);
        false ->
            %% Do not save old connection, that hasn't been used for a long time.
            %% Remove it instead.
            eTodoDB:removeConnection(ConCfg)
    end.

getConnection(User) ->
    case eTodoDB:getConnection(User) of
        undefined -> #conCfg{userName = User};
        DBConCfg  -> DBConCfg
    end.

setConnectData(PeerUser, State = #state{connectData = ConData}) ->
    Ref         = erlang:start_timer(?ConnectMaxTime, self(), connectTimeout),
    ConnectData = [{PeerUser, Ref} | ConData],
    eLog:log(debug, ?MODULE, setConnectData, [ConnectData],
             "Timer started", ?LINE),
    State#state{connectData = ConnectData}.

handleConnectData(PeerUser, State = #state{connectData = ConData}) ->
    case lists:keytake(PeerUser, 1, ConData) of
        false ->
            eLog:log(debug, ?MODULE, handleConnectData, [PeerUser, State],
                     "No matching timer", ?LINE),
            State;
        {value, {PeerUser, Ref}, ConData2} ->
            erlang:cancel_timer(Ref),
            eLog:log(debug, ?MODULE, handleConnectData, [PeerUser],
                     "Timer stopped", ?LINE),
            State#state{connectData = ConData2}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connection "queue" functions.
%%
%% @spec queueAdd(ConOwner, ConList, ConQueue) -> NewConQueue
%% @end
%%--------------------------------------------------------------------

%% Sort configured first, newly used second.
queueAdd(ConOwner, ConList, ConQueue) ->
    SortFun = fun(#conCfg{updateTime = U1}, #conCfg{updateTime = U2}) ->
                      case {U1, U2} of
                          {configured, _} -> true;
                          {_, configured} -> false;
                          _               -> U1 > U2
                      end
              end,
    ConList2 = lists:sort(SortFun, ConList),
    ConQueue ++ [{ConOwner, ConCfg} || ConCfg <- ConList2].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Close down ePeer, when timeout has been received.
%%
%% @spec closeConnectionToPeer(PeerUser) -> void()
%% @end
%%--------------------------------------------------------------------
closeConnectionToPeer(PeerUser) ->
    case ePeerServer:getPeers([PeerUser]) of
        [Pid] ->
            eLog:log(debug, ?MODULE, closeConnectionToPeer, [PeerUser, Pid],
                     "Sending stop to ePeer.", ?LINE),
            ePeer:stop(Pid);
        _ ->
            eLog:log(debug, ?MODULE, closeConnectionToPeer, [PeerUser],
                     "Failed to stop ePeer(not found).", ?LINE)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if peer is active.
%%
%% @spec checkIfActive(User, State) -> true | false
%% @end
%%--------------------------------------------------------------------
checkIfActive(User, State) ->
    case getPeerStatus(User, State) of
        #peer{status = active} ->
            true;
        _ ->
            false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

first_retry_timer_test_() ->
    {timeout, 10,
     fun() ->
             State = #state{user = "local", circle = "circle"},
             Started = erlang:monotonic_time(millisecond),
             Waiting = savePeerStatus("remote", inactive, State),
             Peer = getPeerStatus("remote", Waiting),
             Ref = Peer#peer.retryRef,
             try
                 receive
                     {timeout, Ref, {retryPeer, "remote"}} ->
                         Elapsed = erlang:monotonic_time(millisecond) - Started,
                         ?assert(Elapsed >= 5000),
                         {retry, Ready} = expireRetry("remote", Ref, Waiting),
                         Connection = #conCfg{userName = "remote"},
                         Queued = queueRetryConnection(
                                    Connection, Ready#state{loopActive = true}),
                         ?assertEqual([{"local", Connection}],
                                      Queued#state.conQueue),
                         ?assertEqual(10000,
                                      (getPeerStatus("remote", Queued))#peer.retryDelay)
                 after 6500 ->
                     ?assert(false)
                 end
             after
                 cancelPeerRetry(Peer)
             end
     end}.

retry_backoff_test() ->
    State = #state{user = "local", circle = "circle"},
    lists:foldl(
      fun(Delay, PreviousState) ->
              Connecting = savePeerStatus("remote", connecting, PreviousState),
              Waiting = savePeerStatus("remote", inactive, Connecting),
              #peer{retryDelay = Delay, retryRef = Ref} =
                  getPeerStatus("remote", Waiting),
              Remaining = erlang:read_timer(Ref),
              ?assert(is_integer(Remaining)),
              ?assert(Remaining =< Delay),
              ?assert(Remaining > Delay - 1000),
              erlang:cancel_timer(Ref),
              {retry, Ready} = expireRetry("remote", Ref, Waiting),
              ?assertEqual(min(Delay * 2, 300000),
                           (getPeerStatus("remote", Ready))#peer.retryDelay),
              Ready
      end,
      State,
      [5000, 10000, 20000, 40000, 80000, 160000, 300000, 300000]).

retry_success_resets_test() ->
    State = #state{user = "local", circle = "circle",
                   peers = [#peer{user = "remote", status = connecting,
                                   retryDelay = 300000}]},
    Waiting = savePeerStatus("remote", inactive, State),
    #peer{retryRef = Ref} = getPeerStatus("remote", Waiting),
    Connected = savePeerStatus("remote", active, Waiting),
    ?assertEqual(false, erlang:read_timer(Ref)),
    ?assertMatch(#peer{status = active, retryRef = undefined, retryDelay = 5000},
                 getPeerStatus("remote", Connected)),
    Disconnected = savePeerStatus("remote", inactive, Connected),
    Peer = getPeerStatus("remote", Disconnected),
    ?assertEqual(5000, Peer#peer.retryDelay),
    cancelPeerRetry(Peer).

retry_duplicate_failure_test() ->
    State = #state{user = "local", circle = "circle"},
    Waiting = savePeerStatus("remote", inactive, State),
    ?assertEqual(Waiting, savePeerStatus("remote", inactive, Waiting)),
    ?assertEqual({next, Waiting},
                 connectToPeer(#conCfg{userName = "remote",
                                        host = "localhost", port = 19000},
                                Waiting)),
    cancelPeerRetry(getPeerStatus("remote", Waiting)).

retry_reference_guard_test() ->
    State = #state{user = "local", circle = "circle"},
    Waiting = savePeerStatus("remote", inactive, State),
    Peer = getPeerStatus("remote", Waiting),
    ?assertEqual({ignore, Waiting}, expireRetry("remote", make_ref(), Waiting)),
    Offline = Waiting#state{circle = undefined},
    ?assertEqual({ignore, Offline},
                 expireRetry("remote", Peer#peer.retryRef, Offline)),
    Connected = savePeerStatus("remote", active, Waiting),
    ?assertEqual({ignore, Connected},
                 expireRetry("remote", Peer#peer.retryRef, Connected)).

retry_peers_are_independent_test() ->
    State = #state{user = "local", circle = "circle",
                   peers = [#peer{user = "one", status = connecting,
                                   retryDelay = 160000},
                            #peer{user = "two", status = connecting,
                                   retryDelay = 300000}]},
    Waiting = savePeerStatus("two", inactive,
                             savePeerStatus("one", inactive, State)),
    First = getPeerStatus("one", Waiting),
    Connected = savePeerStatus("two", active, Waiting),
    ?assertEqual(First, getPeerStatus("one", Connected)),
    ?assertEqual(5000, (getPeerStatus("two", Connected))#peer.retryDelay),
    cancelPeerRetry(First).

retry_prioritizes_disconnected_peer_test() ->
    Existing = #conCfg{userName = "other"},
    Remote = #conCfg{userName = "remote"},
    State = #state{user = "local", stateName = proxySetup,
                   loopActive = true, conQueue = [{"local", Existing}]},
    Queued = queueRetryConnection(Remote, State),
    ?assertEqual(directSetup, Queued#state.stateName),
    ?assertEqual([{"local", Remote}, {"local", Existing}], Queued#state.conQueue).

retry_proxy_fallback_preserves_delay_test() ->
    State = #state{user = "local", circle = "circle"},
    Waiting = savePeerStatus("remote", inactive, State),
    #peer{retryRef = Ref} = getPeerStatus("remote", Waiting),
    ProxyConnecting = savePeerStatus("remote", connecting, Waiting),
    ?assertEqual(false, erlang:read_timer(Ref)),
    ProxyFailed = savePeerStatus("remote", inactive, ProxyConnecting),
    Peer = getPeerStatus("remote", ProxyFailed),
    ?assertEqual(5000, Peer#peer.retryDelay),
    cancelPeerRetry(Peer).

retry_ignores_old_circle_status_test() ->
    Ref = make_ref(),
    State = #state{circle = "new", connectData = [{"remote", Ref}]},
    ?assertEqual({noreply, State},
                 handle_cast({peerStatus, "remote", inactive, "old"}, State)).

disconnect_clears_pending_peer_list_test() ->
    State = #state{user = "local", circle = "circle",
                   remoteQueue = ["remote", "other", "remote"]},
    Disconnected = savePeerStatus("remote", inactive, State),
    ?assertEqual(["other"], Disconnected#state.remoteQueue),
    Peer = getPeerStatus("remote", Disconnected),
    Cancelled = cancelPeerRetry(Peer),
    ?assertEqual(false, erlang:read_timer(Peer#peer.retryRef)),
    ?assertEqual(Cancelled, cancelPeerRetry(Cancelled)),
    ?assertMatch(#peer{retryRef = undefined},
                 updatePeerRetry(inactive, Cancelled,
                                  State#state{circle = undefined})).

-endif.
