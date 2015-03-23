%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eTodoEH).

-behaviour(gen_event).

%% API
-export([]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {user, mode}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([{User, noGui}]) ->
    eWeb:start_link(User),
    {ok, #state{user = User, mode = noGui}};
init([User]) ->
    {ok, #state{user = User}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({loggedOut, User}, State = #state{mode = noGui}) ->
    eWeb:loggedOut(User),
    ePluginServer:eLoggedOutMsg(User),
    {ok, State};
handle_event({alarmEntry, Uid, Description}, State = #state{mode = noGui}) ->
    {_, HtmlCSS} = eHtml:generateAlarmMsg(Uid, Description),
    eWeb:appendToPage(HtmlCSS),
    ePluginServer:eReceivedAlarmMsg(Description),
    {ok, State};
handle_event({loggedIn, User}, State) ->
    eTodo:loggedIn(User),
    eWeb:loggedIn(User),
    ePluginServer:eLoggedInMsg(User),
    {ok, State};
handle_event({loggedOut, User}, State) ->
    eTodo:loggedOut(User),
    eWeb:loggedOut(User),
    ePluginServer:eLoggedOutMsg(User),
    {ok, State};
handle_event({alarmEntry, Uid, Description}, State) ->
    eTodo:alarmEntry(Uid, Description),
    {_, HtmlCSS} = eHtml:generateAlarmMsg(Uid, Description),
    eWeb:appendToPage(HtmlCSS),
    ePluginServer:eReceivedAlarmMsg(Description),
    {ok, State};
handle_event({sendMsg, Sender, Users, msgEntry, Text},
             State = #state{user = Sender}) ->
    {_, HtmlCSS} = eHtml:generateMsg(Sender, Sender, Users, Text),
    eWeb:appendToPage(HtmlCSS),
    {ok, State};
handle_event({sendMsg, Sender, Users, MsgType, Text}, State) ->
    case lists:member(State#state.user, Users) of
        true ->
            sendMsg(Sender, Users, MsgType, Text, State);
        false ->
            ok
    end,
    {ok, State};
handle_event({todoUpdated, Sender, _Users, _Todo},
             State = #state{user = Sender}) ->
    ok,
    {ok, State};
handle_event({todoUpdated, Sender, Users, Todo}, State) ->
    case lists:member(State#state.user, Users) of
        true ->
            eTodo:todoUpdated(Sender, Todo);
        false ->
            ok
    end,
    {ok, State};
handle_event(_Msg, State = #state{mode = noGui}) ->
    {ok, State};
handle_event({checkConflict, User, PeerUser,
              OldLocalConTime, OldRemoteConTime, Diff},
             State = #state{user = User}) ->
    eTodo:checkConflict(User, PeerUser,
                        OldLocalConTime, OldRemoteConTime, Diff),
    {ok, State};
handle_event({acceptingIncCon, User, Circle, Port}, State) ->
    eTodo:acceptingIncCon(User, Circle, Port),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
%% send message to GUI
%%
%% @spec sendMsg(Sender, MsgType, Text) -> ok
%% @end
%%--------------------------------------------------------------------
sendMsg(_Sender, _Users, statusEntry,
        {statusUpdate, UserStatus, Avatar}, State) ->
    sendToGui(statusUpdate, [UserStatus, Avatar], State);
sendMsg(_Sender, _Users, statusEntry, {statusUpdate, UserStatus}, State) ->
    sendToGui(statusUpdate, [UserStatus, undefined], State);
sendMsg(Sender, _Users, statusEntry, writing, State) ->
    sendToGui(writing, [Sender], State);
sendMsg(Sender, Users, msgEntry, Text, State = #state{user = User}) ->
    sendToGui(msgEntry, [Sender, Users, Text], State),
    {_, HtmlCSS} = eHtml:generateMsg(User, Sender, Users, Text),
    eWeb:appendToPage(HtmlCSS),
    ePluginServer:eReceivedMsg(Sender, Users, Text);
sendMsg(Sender, _Users, systemEntry, Text, State) ->
    sendToGui(systemEntry, [Sender, Text], State),
    {_, HtmlCSS} = eHtml:generateSystemMsg(Sender, Text),
    eWeb:appendToPage(HtmlCSS),
    ePluginServer:eReceivedSysMsg(Text);
sendMsg(_Sender, _Users, {systemEntry, Uid}, Text, State) ->
    sendToGui(systemEntry, [Uid, Text], State),
    {_, HtmlCSS} = eHtml:generateSystemMsg(Uid, Text),
    eWeb:appendToPage(HtmlCSS),
    ePluginServer:eReceivedSysMsg(Text).

sendToGui(_Function, _Args, #state{mode = noGui}) ->
    ok;
sendToGui(Function, Args, _State) ->
    apply(eTodo, Function, Args).

