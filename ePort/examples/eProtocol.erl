%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@dunderbo.nu>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Mikael Bylund
%%%-------------------------------------------------------------------
-module(eProtocol).

%% API
-export([]]).

%% ePort messages
-export([clientConnected/3,
         clientDisconnected/2,
         info/6
        ]).

-define(protSrvModule, eProtocolServer).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% API used by eport to generate error logs.
%% @end
%%--------------------------------------------------------------------
-spec(info(LogLevel    :: atom(),
           Module      :: atom(),
           Function    :: atom(),
           Args        :: [term()],
           ErrorDesc   :: string(),
           LineNumber  :: integer()) -> ok).

info(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    eLog:log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber).

%%--------------------------------------------------------------------
%% @doc
%% Someone connected to the eTodo listener.
%% @spec clientConnected(EPortPid, EListenerPid, PeerHost) -> ok
%% @end
%%--------------------------------------------------------------------

clientConnected(EPortPid, EListenerPid, PeerHost) ->
    ?protSrvModule:clientConnected(EPortPid, EListenerPid, PeerHost).

%%--------------------------------------------------------------------
%% @doc
%% The connection to an eTodo socket was lost.
%% @spec clientDisconnected(EPortPid, EListenerPid) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(clientDisconnected(EPortPid      :: pid(),
                         ProtVersion   :: string()) -> ok).

clientDisconnected(EPortPid, EListenerPid) ->
    ?protSrvModule:clientDisconnected(EPortPid, EListenerPid).

