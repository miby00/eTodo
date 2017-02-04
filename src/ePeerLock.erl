%%%-------------------------------------------------------------------
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @author Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%%-------------------------------------------------------------------
-module(ePeerLock).

-behaviour(gen_server).

%% API
-export([start_link/0,
         getLock/2,
         getLocks/3,
         releaseLock/2,
         releaseLocks/3,
         loggedIn/1,
         loggedOut/1]).

-export([doGetLocks/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {locks = [], queue = [], users = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get lock. Will generate exit timeout signal if lock is unachivable
%% @spec getLock(Uid, Owner) -> ok
%% @end
%%--------------------------------------------------------------------
getLock(Uid, Owner) ->
    gen_server:call(?MODULE, {getLock, Uid, Owner}).

%%--------------------------------------------------------------------
%% @doc
%% Release lock
%% @spec releaseLock(Uid, Owner) -> ok
%% @end
%%--------------------------------------------------------------------
releaseLock(Uid, Owner) ->
    gen_server:cast(?MODULE, {releaseLock, Uid, Owner}).

%%--------------------------------------------------------------------
%% @doc
%% Release locks on multiple peers
%% @spec releaseLocks(Uid, Owner, Users) -> ok
%% @end
%%--------------------------------------------------------------------
releaseLocks(Uid, Owner, Users) ->
    gen_server:cast(?MODULE, {releaseLocks, Uid, Owner, Users}).

%%--------------------------------------------------------------------
%% @doc
%% Signal received from ePeerLockEH when someone logs out.
%% Needed so we can release lock when someone disapears.
%% @spec loggedOut(User) -> ok
%% @end
%%--------------------------------------------------------------------
loggedIn(User) ->
    gen_server:cast(?MODULE, {loggedIn, User}).

%%--------------------------------------------------------------------
%% @doc
%% Signal received from ePeerLockEH when someone logs out.
%% Needed so we can release lock when someone disapears.
%% @spec loggedOut(User) -> ok
%% @end
%%--------------------------------------------------------------------
loggedOut(User) ->
    gen_server:cast(?MODULE, {loggedOut, User}).

%%--------------------------------------------------------------------
%% @doc
%% Get lock on multiple peers.
%% @spec getLocks(Uid, Owner, Users) -> ok
%% @end
%%--------------------------------------------------------------------
getLocks(Uid, Owner, Users) ->
    gen_server:call(?MODULE, {getLocks, Uid, Owner, Users}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ePeerEM:add_handler(ePeerLockEH, []),
    {ok, #state{}}.

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
handle_call({getLock, Uid, Owner}, From, State) ->
    doGetLock(Uid, Owner, From, State);
handle_call({getLocks, Uid, Owner, []}, From, State) ->
    doGetLock(Uid, Owner, From, State);
handle_call({getLocks, Uid, Owner, [Owner]}, From, State) ->
    doGetLock(Uid, Owner, From, State);
handle_call({getLocks, Uid, Owner, Users}, From,
            State = #state{users = LoggedIn}) ->
    LoggedInShared = (Users -- (Users -- LoggedIn)),
    spawn(?MODULE, doGetLocks, [Uid, Owner, LoggedInShared, From]),
    {noreply, State};
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
handle_cast({releaseLock, Uid, _Owner}, State) ->
    doReleaseLock(Uid, State);
handle_cast({releaseLocks, Uid, _Owner, []}, State) ->
    doReleaseLock(Uid, State);
handle_cast({releaseLocks, Uid, Owner, [Owner]}, State) ->
    doReleaseLock(Uid, State);
handle_cast({releaseLocks, Uid, Owner, Users},
            State = #state{users = LoggedIn}) ->
    LoggedInShared = (Users -- (Users -- LoggedIn)),
    Peers = ePeerServer:getPeers(LoggedInShared),
    [ePeer:releaseLock(EPid, Uid, Owner) || EPid <- Peers],
    releaseLock(Uid, Owner),
    {noreply, State};
handle_cast({loggedIn, User}, State= #state{users = Users}) ->
    {noreply, State#state{users = [User|Users]}};
handle_cast({loggedOut, User}, State= #state{locks = Locks,
                                             users = Users}) ->
    doReleaseLocks(User, Locks),
    {noreply, State#state{users = lists:delete(User, Users)}};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get lock from multiple peers
%%
%% @spec doGetLocks(Uid, Owner, Users, From) -> ok
%% @end
%%--------------------------------------------------------------------
doGetLocks(Uid, Owner, Users, From) ->
    Active = [Peer || Peer <- Users, ePeerCircle:isActivePeer(Peer)],
    Peers = ePeerServer:getPeers(Active),
    [ePeer:getLock(EPid, Uid, Owner) || EPid <- Peers],
    ePeerLock:getLock(Uid, Owner),
    gen_server:reply(From, ok).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release lock from multiple peers
%%
%% @spec doReleaseLocks(User, Locks) -> ok
%% @end
%%--------------------------------------------------------------------
doReleaseLocks(_User, []) ->
    ok;
doReleaseLocks(User, [{Uid, User}|Locks]) ->
    releaseLock(Uid, User),
    doReleaseLocks(User, Locks);
doReleaseLocks(User, [{_Uid, _Owner}|Locks]) ->
    doReleaseLocks(User, Locks).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get lock
%%
%% @spec doGetLock(Uid, Owner, From, State) -> {reply, ok, State} |
%%                                             {noreply, State}
%% @end
%%--------------------------------------------------------------------
doGetLock(Uid, Owner, From, State = #state{locks = Locks, queue = Queue}) ->
    case lists:keyfind(Uid, 1, Locks) of
        {Uid, Owner} ->
            %% Already have a lock, write on...
            {reply, ok, State};
        {Uid, _} ->
            %% Someone else have a lock, queue...
            {noreply, State#state{queue = Queue ++ [{Uid, From, Owner}]}};
        false ->
            %% No one have a lock, register lock.
            {reply, ok, State#state{locks = [{Uid, Owner}|Locks]}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release lock
%%
%% @spec doReleaseLock(Uid, State) -> {noreply, State}
%% @end
%%--------------------------------------------------------------------
doReleaseLock(Uid, State = #state{locks = Locks, queue = Queue}) ->
    case lists:keyfind(Uid, 1, Queue) of
        false ->
            {noreply, State#state{locks = lists:keydelete(Uid, 1, Locks)}};
        {Uid, From, _} ->
            gen_server:reply(From, ok),
            Queue2 = lists:keydelete(Uid, 1, Queue),
            {noreply, State#state{queue = Queue2}}
    end.
