%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%%-------------------------------------------------------------------
-module(ePeerServer).

-behaviour(gen_server).

%% API
-export([start_link/0,
         connectedToCircle/2,
         loggedOutFromCircle/0,
         clientConnected/3,
         clientDisconnected/2,
         peerCall/3,
         peerCall/4,
         peerCast/3,
         proxyCall/3,
         proxyCast/3,
         connectToPeer/5,
         proxyToPeer/3,
         proxyLogin/3,
         updatePeerInfo/2,
         getPeers/1
        ]).

%% Worker process functions
-export([doConnectToPeer/7,
         doConnectReply/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(peer, {pid,
               peerUser,
               ePortPid,
               eListenerPid
              }).

-record(state, {user,
                circle,
                peers = [],
                parent}).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self()], []).

%%--------------------------------------------------------------------
%% @doc
%% Register active circle.
%%
%% @spec connectedToCircle(User, Circle) -> ok
%% @end
%%--------------------------------------------------------------------
connectedToCircle(User, Circle) ->
    gen_server:cast(?SERVER, {connectedToCircle, User, Circle}).

%%--------------------------------------------------------------------
%% @doc
%% Remote current circle.
%%
%% @spec loggedOutFromCircle() -> ok
%% @end
%%--------------------------------------------------------------------
loggedOutFromCircle() ->
    gen_server:cast(?SERVER, loggedOutFromCircle).

%%--------------------------------------------------------------------
%% @doc
%% Someone connected to the eTodo listener.
%% @spec clientConnected(EPortPid, EListenerPid, PeerHost) -> ok
%% @end
%%--------------------------------------------------------------------
clientConnected(EPortPid, EListenerPid, PeerHost) ->
    gen_server:cast(?SERVER,
                    {clientConnected, EPortPid, EListenerPid, PeerHost}).

%%--------------------------------------------------------------------
%% @doc
%% The connection to an eTodo socket was lost.
%% @spec clientDisconnected(EPortPid, EListenerPid) -> ok
%% @end
%%--------------------------------------------------------------------
clientDisconnected(EPortPid, EListenerPid) ->
    gen_server:cast(?SERVER,
                    {clientDisconnected, EPortPid, EListenerPid}).

%%--------------------------------------------------------------------
%% @doc
%% Handle call from a another peer.
%% @spec peerCall(EPortPid, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
peerCall(EPortPid, Message, Args) ->
    gen_server:call(?SERVER,
                    {peerCall, EPortPid, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Handle call from a another peer.
%% @spec peerCall(EPortPid, Message, Args, Timeout) -> ok
%% @end
%%--------------------------------------------------------------------
peerCall(EPortPid, Message, Args, Timeout) ->
    gen_server:call(?SERVER,
                    {peerCall, EPortPid, Message, Args}, Timeout).

%%%--------------------------------------------------------------------
%% @doc
%% Handle cast from a another peer.
%% @spec peerCast(EPortPid, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
peerCast(EPortPid, Message, Args) ->
    gen_server:cast(?SERVER,
                    {peerCast, EPortPid, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Handle call from a another peer.
%% @spec proxyCall(PeerUser, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
proxyCall(PeerUser, Message, Args) ->
    gen_server:call(?SERVER,
                    {proxyCall, PeerUser, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Handle cast from a another peer.
%% @spec proxyCast(PeerUser, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
proxyCast(PeerUser, Message, Args) ->
    gen_server:cast(?SERVER,
                    {proxyCast, PeerUser, Message, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Connect to remote peer.
%% @spec connectToPeer(PeerHost, PeerPort, User, Host, Port) -> ok
%% @end
%%--------------------------------------------------------------------
connectToPeer(PeerUser, PeerHost, PeerPort, User, Port) ->
    gen_server:cast(?SERVER,
                    {connectToPeer, PeerUser, PeerHost, PeerPort, User, Port}).

%%--------------------------------------------------------------------
%% @doc
%% Launch proxy to remote peer.
%% @spec proxyToPeer(PeerUser, ProxyUser, User) -> ok
%% @end
%%--------------------------------------------------------------------
proxyToPeer(PeerUser, ProxyUser, User) ->
    gen_server:cast(?SERVER,
                    {proxyToPeer, PeerUser, ProxyUser, User}).

%%--------------------------------------------------------------------
%% @doc
%% Launch proxy from remote peer.
%% @spec proxyLogin(PeerUser, ProxyUser, User) -> ok
%% @end
%%--------------------------------------------------------------------
proxyLogin(PeerUser, ProxyUser, User) ->
    gen_server:cast(?SERVER,
                    {proxyLogin, PeerUser, ProxyUser, User}).

%%--------------------------------------------------------------------
%% @doc
%% Reply from doConnectToPeer.
%% @spec doConnectReply(Pid, PeerUser, EPortPid) -> ok
%% @end
%%--------------------------------------------------------------------
doConnectReply(Pid, PeerUser, EPortPid) ->
    gen_server:cast(?SERVER,
                    {doConnectReply, Pid, PeerUser, EPortPid}).

%%--------------------------------------------------------------------
%% @doc
%% Set peer user name for ePeer.
%% @spec updatePeerInfo(Pid, PeerUser) -> ok
%% @end
%%--------------------------------------------------------------------
updatePeerInfo(Pid, PeerUser) ->
    gen_server:cast(?SERVER, {updatePeerInfo, Pid, PeerUser}).

%%--------------------------------------------------------------------
%% @doc
%% Get pids for peers. Used by lock server.
%% @spec getPeers(PeerUsers) -> [Pid]
%% @end
%%--------------------------------------------------------------------
getPeers(PeerUsers) ->
    gen_server:call(?SERVER, {getPeers, PeerUsers}).

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
init([Parent]) ->
    process_flag(trap_exit, true),
    eLog:log(debug, ?MODULE, init, [self(), Parent], "Started", ?LINE),
    {ok, #state{parent = Parent}}.

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
handle_call({getPeers, _}, _From, State = #state{circle = undefined}) ->
    %% No peers when not logged in to a circle.
    Pids = [],
    {reply, Pids, State};
handle_call({getPeers, PeerUsers}, _From, State) ->
    Pids = getPeers(PeerUsers, State),
    {reply, Pids, State};
handle_call(_Message, _From, State = #state{circle = undefined}) ->
    %% Messages aren't accepted when not logged in to a circle.
    eLog:log(error, ?MODULE, handle_call, [_Message, State],
             "Message received when not logged in to circle", ?LINE),
    {reply, {error, wrongState}, State};
handle_call({peerCall, EPortPid, Message, Args}, From, State) ->
    case getPeer(ePortPid, EPortPid, State) of
        none ->
            eLog:log(error, ?MODULE, peerCall, [EPortPid, Message],
                     "Message from unknown client", ?LINE),
            {reply, {error, connectFailed}, State};
        Peer ->
            ePeer:peerCall(Peer#peer.pid, From, Message, Args),
            {noreply, State}
    end;
handle_call({proxyCall, PeerUser, Message, Args}, From, State) ->
    case getPeer(peerUser, PeerUser, State) of
        none ->
            eLog:log(error, ?MODULE, proxyCall, [PeerUser, Message],
                     "Message from unknown client", ?LINE),
            {reply, {error, connectFailed}, State};
        Peer ->
            ePeer:peerCall(Peer#peer.pid, From, Message, Args),
            {noreply, State}
    end;
handle_call(_Request, _From, State) ->
    eLog:log(error, ?MODULE, handle_call, [_Request, _From, State],
             "unknown command", ?LINE),
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
handle_cast({connectedToCircle, User, Circle}, State) ->
    {noreply, State#state{user   = User,
                          circle = Circle}};
handle_cast(loggedOutFromCircle, State) ->
    [ePeer:stop(Peer#peer.pid) || Peer <- State#state.peers],
    {noreply, State#state{peers  = [],
                          user   = undefined,
                          circle = undefined}};
handle_cast({clientConnected, EPortPid, EListenerPid, PeerHost},
            State = #state{circle = undefined}) ->
    %% Attempt to connect from remote peer when not logged in - stop it.
    eLog:log(debug, ?MODULE, clientConnected,
             [self(), EPortPid, EListenerPid, PeerHost, State],
             "Denied connection from peer - not logged in to circle", ?LINE),
    ePort:stop(EPortPid),
    {noreply, State};
handle_cast({clientConnected, EPortPid, EListenerPid, PeerHost},
            State = #state{circle = Circle,
                           user   = User}) ->
    eLog:log(debug, ?MODULE, clientConnected,
             [self(), EPortPid], "entering", ?LINE),
    Args      = {EPortPid, EListenerPid, PeerHost, Circle, User},
    {ok, Pid} = ePeer:start(self(), clientConnected, Args),
    Peer      = #peer{pid          = Pid,
                      ePortPid     = EPortPid,
                      eListenerPid = EListenerPid},
    State2 = savePeer(Peer, State),
    link(Pid),
    eLog:log(debug, ?MODULE, clientConnected, [self(), State2], "done", ?LINE),
    {noreply, State2};
handle_cast(_Message, State = #state{circle = undefined}) ->
    %% Messages aren't accepted when not logged in to a circle.
    eLog:log(error, ?MODULE, handle_cast, [_Message, State],
             "Message received when not logged in to circle", ?LINE),
    {noreply, State};
handle_cast({clientDisconnected, EPortPid, _EListenerPid}, State) ->
    eLog:log(debug, ?MODULE, clientDisconnected,
             [self(), EPortPid], "entering", ?LINE),
    State2 =
        case getPeer(ePortPid, EPortPid, State) of
            none ->
                eLog:log(debug, ?MODULE, clientDisconnected, [EPortPid],
                         "Unknown client disconnected", ?LINE),
                State;
            Peer ->
                ePeer:stop(Peer#peer.pid),
                deletePeer(Peer, State)
        end,
    eLog:log(debug, ?MODULE, clientDisconnected,
             [self(), State2], "done", ?LINE),
    {noreply, State2};
handle_cast({connectToPeer, PeerUser, PeerHost, PeerPort, User, Port},
            State = #state{circle = Circle}) ->
    eLog:log(debug, ?MODULE, connectToPeer,
             [PeerUser, PeerHost, PeerPort, User, Port],
             "launching worker", ?LINE),
    spawn(?MODULE, doConnectToPeer,
          [self(), PeerUser, PeerHost, PeerPort, User, Port, Circle]),
    {noreply, State};
handle_cast({doConnectReply, Pid, PeerUser, EPortPid}, State) ->
    Peer      = #peer{pid      = Pid,
                      peerUser = PeerUser,
                      ePortPid = EPortPid},
    State2    = savePeer(Peer, State),
    link(Pid),
    eLog:log(debug, ?MODULE, connectToPeer, [State2], "done", ?LINE),
    {noreply, State2};
handle_cast({proxyToPeer, PeerUser, ProxyUser, User},
            State = #state{circle = Circle}) ->
    case getPeer(peerUser, ProxyUser, State) of
        none ->
            eLog:log(error, ?MODULE, proxyToPeer, [PeerUser, ProxyUser, User],
                     "Proxy user not found", ?LINE),
            ePeerCircle:peerStatus(PeerUser, inactive, Circle),
            {noreply, State};
        ProxyPeer ->
            ProxyPid  = ProxyPeer#peer.pid,
            Args      = {PeerUser, ProxyPid, User, Circle},
            {ok, Pid} = ePeer:start(self(), proxyToPeer, Args),
            Peer      = #peer{pid      = Pid,
                              peerUser = PeerUser},
            State2    = savePeer(Peer, State),
            link(Pid),
            eLog:log(debug, ?MODULE, proxyToPeer, [PeerUser, ProxyUser, User],
                     "Proxy ePeer started", ?LINE),
            {noreply, State2}
    end;
handle_cast({proxyLogin, PeerUser, ProxyUser, User},
            State = #state{circle = Circle}) ->
    case getPeer(peerUser, ProxyUser, State) of
        none ->
            eLog:log(error, ?MODULE, proxyToPeer, [PeerUser, ProxyUser, User],
                     "Proxy user not found", ?LINE),
            {noreply, State};
        ProxyPeer ->
            ProxyPid  = ProxyPeer#peer.pid,
            Args      = {PeerUser, ProxyPid, User, Circle},
            {ok, Pid} = ePeer:start(self(), proxyLogin, Args),
            Peer      = #peer{pid      = Pid,
                              peerUser = PeerUser},
            State2    = savePeer(Peer, State),
            link(Pid),
            eLog:log(debug, ?MODULE, proxyLogin, [PeerUser, ProxyUser, User],
                     "Proxy ePeer started", ?LINE),
            {noreply, State2}
    end;
handle_cast({peerCast, EPortPid, Message, Args}, State) ->
    case getPeer(ePortPid, EPortPid, State) of
        none ->
            eLog:log(error, ?MODULE, peerCast, [EPortPid, Message],
                     "Message from unknown client", ?LINE);
        Peer ->
            ePeer:peerCast(Peer#peer.pid, Message, Args)
    end,
    {noreply, State};
handle_cast({proxyCast, PeerUser, Message, Args}, State) ->
    case getPeer(peerUser, PeerUser, State) of
        none ->
            eLog:log(error, ?MODULE, proxyCast, [PeerUser, Message],
                     "Message from unknown client", ?LINE);
        Peer ->
            ePeer:peerCast(Peer#peer.pid, Message, Args)
    end,
    {noreply, State};
handle_cast({updatePeerInfo, Pid, PeerUser}, State) ->
    Peer   = getPeer(pid, Pid, State),
    State2 = savePeer(Peer#peer{peerUser = PeerUser}, State),
    {noreply, State2};
handle_cast(_Msg, State) ->
    eLog:log(error, ?MODULE, handle_call, [_Msg, State],
             "unknown command", ?LINE),
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
handle_info({'EXIT', Parent, Reason}, State = #state{parent = Parent}) ->
    {stop, Reason, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    eLog:log(debug, ?MODULE, handle_info, [Pid], "peer lost", ?LINE),
    State2 = case getPeer(pid, Pid, State) of
                 none -> State;
                 Peer -> deletePeer(Peer, State)
             end,
    {noreply, State2};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Worker for establishing a connection.
%%
%% @spec doConnectToPeer(EPeerServerPid, PeerUser, PeerHost, PeerPort,
%%                       User, Port, Circle) -> ok
%% @end
%%--------------------------------------------------------------------
doConnectToPeer(EPeerServerPid, PeerUser, PeerHost, PeerPort,
                User, Port, Circle) ->
    case ePort:start(ePeerProtocol, PeerHost, PeerPort, []) of
        {ok, EPortPid} ->
            Args      = {EPortPid, PeerUser, PeerHost, PeerPort,
                         User, Port, Circle},
            {ok, Pid} = ePeer:start(EPeerServerPid, connectToPeer, Args),
            doConnectReply(Pid, PeerUser, EPortPid),
            ok;
        _Else ->
            eLog:log(debug, ?MODULE, doConnectToPeer,
                     [_Else, self(), PeerUser], "failed to start ePort", ?LINE),
            ePeerCircle:peerStatus(PeerUser, inactive, Circle),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Look up a peer in state
%%
%% @spec getPeer(Key, Value, State) -> Peer | none
%% @end
%%--------------------------------------------------------------------
getPeer(pid,      Pid,      #state{peers = Peers}) ->
    case lists:keysearch(Pid, #peer.pid, Peers) of
        {value, Peer} -> Peer;
        _Else         -> none
    end;
getPeer(ePortPid, EPortPid, #state{peers = Peers}) ->
    case lists:keysearch(EPortPid, #peer.ePortPid, Peers) of
        {value, Peer} -> Peer;
        _Else         -> none
    end;
getPeer(peerUser, PeerUser, #state{peers = Peers}) ->
    case lists:keysearch(PeerUser, #peer.peerUser, Peers) of
        {value, Peer} -> Peer;
        _Else         -> none
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save a peer in state
%%
%% @spec savePeer(Peer, State) -> NewState
%% @end
%%--------------------------------------------------------------------
savePeer(Peer, State = #state{peers = Peers}) when is_record(Peer, peer) ->
    Peers2 = lists:keystore(Peer#peer.pid, #peer.pid, Peers, Peer),
    State#state{peers = Peers2};
savePeer(Peer, State) ->
    eLog:log(error, ?MODULE, savePeer, [self(), Peer],
             "Unexpected peer, ignore.", ?LINE),
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove a peer from state
%%
%% @spec deletePeer(Peer, State) -> NewState
%% @end
%%--------------------------------------------------------------------
deletePeer(Peer, State = #state{peers = Peers}) ->
    Peers2 = lists:keydelete(Peer#peer.pid, #peer.pid, Peers),
    State#state{peers = Peers2}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get list of Pids for a list of peer users.
%%
%% @spec getPeers(PeerUsers, State) -> [Pid]
%% @end
%%--------------------------------------------------------------------
getPeers(PeerUsers, #state{peers = Peers}) ->
    getPeers(PeerUsers, Peers, []).

getPeers([], _, Acc) ->
    Acc;
getPeers([PeerUser | PeerUsers], Peers, Acc) ->
    case lists:keyfind(PeerUser, #peer.peerUser, Peers) of
        #peer{pid = Pid} ->
            getPeers(PeerUsers, Peers, [Pid | Acc]);
        false ->
            getPeers(PeerUsers, Peers, Acc)
    end.
