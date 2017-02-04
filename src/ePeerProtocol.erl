%%%-------------------------------------------------------------------
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @author Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%%-------------------------------------------------------------------
-module(ePeerProtocol).

%% API
-export([eTodoConnect/5,
         eTodoRegister/5,
         sendMsg/5,
         todoUpdated/4,
         getLock/3,
         webProxyCall/4,
         releaseLock/3,

         proxyMsg/6
        ]).

%% ePort messages
-export([clientConnected/3,
         clientDisconnected/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Someone connected to the eTodo listener.
%% @spec clientConnected(EPortPid, EListenerPid, PeerHost) -> ok
%% @end
%%--------------------------------------------------------------------
clientConnected(EPortPid, EListenerPid, PeerHost) ->
    ePeerServer:clientConnected(EPortPid, EListenerPid, PeerHost).

%%--------------------------------------------------------------------
%% @doc
%% The connection to an eTodo socket was lost.
%% @spec clientDisconnected(EPortPid, EListenerPid) -> ok
%% @end
%%--------------------------------------------------------------------
clientDisconnected(EPortPid, EListenerPid) ->
    ePeerServer:clientDisconnected(EPortPid, EListenerPid).

%%--------------------------------------------------------------------
%% @doc
%% Connect to a peer
%% @spec eTodoConnect() -> Result
%% @end
%%--------------------------------------------------------------------
eTodoConnect(EPortPid, User, PeerUser, PeerPort, Timestamp) ->
    ePeerServer:peerCall(EPortPid, eTodoConnect,
                         [User, PeerUser, PeerPort, Timestamp]).

%%--------------------------------------------------------------------
%% @doc
%% Register with a peer
%% @spec eTodoRegister() -> Result
%% @end
%%--------------------------------------------------------------------
eTodoRegister(EPortPid, PeerUser, Hash, DateTime, Peers) ->
    ePeerServer:peerCall(EPortPid, eTodoRegister,
                         [PeerUser, Hash, DateTime, Peers]).

%%--------------------------------------------------------------------
%% @doc
%% Send a chat message to a peer.
%% @spec sendMsg(User, PeerUsers, MsgType, Text) -> Result
%% @end
%%--------------------------------------------------------------------
sendMsg(EPortPid, User, PeerUsers, MsgType, Text) ->
    ePeerServer:peerCast(EPortPid, sendMsg, [User, PeerUsers, MsgType, Text]).

%%--------------------------------------------------------------------
%% @doc
%% Update task for user
%% @spec todoUpdated(User, PeerUser, Task) -> Result
%% @end
%%--------------------------------------------------------------------
todoUpdated(EPortPid, User, PeerUser, Task) ->
    ePeerServer:peerCast(EPortPid, todoUpdated, [User, PeerUser, Task]).

%%--------------------------------------------------------------------
%% @doc
%% Get lock
%% @spec getLock(Uid, Owner) -> ok
%% @end
%%--------------------------------------------------------------------
getLock(EPortPid, Uid, Owner) ->
    ePeerServer:peerCall(EPortPid, getLock, [Uid, Owner]).

%%--------------------------------------------------------------------
%% @doc
%% Release lock
%% @spec releaseLock(Uid, Owner) -> ok
%% @end
%%--------------------------------------------------------------------
releaseLock(EPortPid, Uid, Owner) ->
    ePeerServer:peerCast(EPortPid, releaseLock, [Uid, Owner]).

%%--------------------------------------------------------------------
%% @doc
%% Get lock
%% @spec webProxyCall(Message, HashPwd, Timeout) -> Result
%% @end
%%--------------------------------------------------------------------
webProxyCall(EPortPid, Message, HashPwd, Timeout) ->
    ePeerServer:peerCall(EPortPid, webProxyCall,
                         [Message, HashPwd, Timeout], Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Relay a messsage to another peer.
%% @spec proxyMsg(Type, SrcUser, DestUser, Message, Args) -> ok
%% @end
%%--------------------------------------------------------------------
proxyMsg(EPortPid, call, SrcUser, DestUser, Message, Args) ->
    case validMessage(Message) of
        true ->
            ePeerServer:peerCall(EPortPid,
                                 proxyMsg, [SrcUser, DestUser, Message, Args]);
        false ->
            eLog:log(error, ?MODULE, proxyMsg,
                     [call, SrcUser, DestUser, Message, Args],
                     "Message not allowed through proxy", ?LINE),
            ok
    end;
proxyMsg(EPortPid, cast, SrcUser, DestUser, Message, Args) ->
    case validMessage(Message) of
        true ->
            ePeerServer:peerCast(EPortPid,
                                 proxyMsg, [SrcUser, DestUser, Message, Args]);
        false ->
            eLog:log(error, ?MODULE, proxyMsg,
                     [cast, SrcUser, DestUser, Message, Args],
                     "Message not allowed through proxy", ?LINE),
            ok
    end.

validMessage(eTodoConnect)    -> true;
validMessage(eTodoRegister)   -> true;
validMessage(sendMsg)         -> true;
validMessage(todoUpdated)     -> true;
validMessage(getLock)         -> true;
validMessage(webProxyCall)    -> true;
validMessage(releaseLock)     -> true;
validMessage(proxyMsg)        -> false; %% Don't proxy a proxy.
validMessage(proxyLogin)      -> true;  %% Only available in proxyMsg.
validMessage(proxyLoginReply) -> true;  %% Only available in proxyMsg.
validMessage(ping)            -> true;  %% Only available in proxyMsg.
validMessage(_Message)        -> false. %% Only allow listed messages.
