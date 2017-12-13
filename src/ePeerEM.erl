%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Mikael Bylund <mikael.bylund.2726@student.uu.se>
%%%-------------------------------------------------------------------
-module(ePeerEM).

%% API
-export([start_link/0,
         add_handler/2,
         del_handler/2,
         acceptingIncCon/3,
         connectToCircle/3,
         todoUpdated/3,
         loggedIn/1,
         loggedOut/1,
         msgEntry/3,
         sendMsg/4,
         alarmEntry/2,
         checkConflict/5]).

%% gen_event callbacks
-export([]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler(HandlerId, Args) -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, [Args]).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec del_handler(HandlerId, Args) -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
del_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, [Args]).

%%--------------------------------------------------------------------
%% @doc
%% Send event to login on circle
%%
%% @spec connectToCircle(User, Circle, Password) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
connectToCircle(User, Circle, Password) ->
    gen_event:notify(?SERVER, {connectToCircle, User, Circle, Password}).

%%--------------------------------------------------------------------
%% @doc
%% Report back which listener that was created.
%%
%% @spec acceptIncCon(User, Circle, Port) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
acceptingIncCon(User, Circle, Port) ->
    gen_event:notify(?SERVER, {acceptingIncCon, User, Circle, Port}).

%%--------------------------------------------------------------------
%% @doc
%% Send event to login on circle
%%
%% @spec todoUpdated(Sender, Task) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
todoUpdated(Sender, Users, Task) ->
    gen_event:notify(?SERVER, {todoUpdated, Sender, Users, Task}).

%%--------------------------------------------------------------------
%% @doc
%% Send event that tells us a user logged in
%%
%% @spec loggedIn(User) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
loggedIn(User) ->
    gen_event:notify(?SERVER, {loggedIn, User}).

%%--------------------------------------------------------------------
%% @doc
%% Send event that tells us a user logged in
%%
%% @spec loggedOut(User) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
loggedOut(User) ->
    gen_event:notify(?SERVER, {loggedOut, User}).

%%--------------------------------------------------------------------
%% @doc
%% Send msg to users
%%
%% @spec sendMsg(Sender, Users, MsgType, Text) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
sendMsg(Sender, Users, MsgType, Text) ->
    gen_event:notify(?SERVER, {sendMsg, Sender, Users, MsgType, Text}).

%%--------------------------------------------------------------------
%% @doc
%% Send msg to users
%%
%% @spec msgEntry(Sender, Users, Text) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
msgEntry(Sender, Users, Text) ->
    gen_event:notify(?SERVER, {msgEntry, Sender, Users, Text}).

%%--------------------------------------------------------------------
%% @doc
%% Send alarm to be shown in GUI
%%
%% @spec alarmEntry(Uid, Description) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
alarmEntry(Uid, Description) ->
    gen_event:notify(?SERVER, {alarmEntry, Uid, Description}).

%%--------------------------------------------------------------------
%% @doc
%% Todos needed to be conflict solved.
%%
%% @spec checkConflict(User, PeerUser,
%%                     OldLocalConTime, OldRemoteConTime, Diff) ->
%%       ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
checkConflict(User, PeerUser,
              OldLocalConTime, OldRemoteConTime, Diff) ->
    gen_event:notify(?SERVER, {checkConflict, User, PeerUser,
                               OldLocalConTime, OldRemoteConTime, Diff}).

