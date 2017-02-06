%%%-------------------------------------------------------------------
%%% @author mikael <mikael@MIKAEL-PC>
%%% @copyright (C) 2012, mikael
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2012 by mikael <mikael@MIKAEL-PC>
%%%-------------------------------------------------------------------
-module(startETodo).

%% API
-export([gui/0, noGui/3, noGui/5, noGui/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
noGui([User, Circle, Pwd]) ->
    noGui(User, Circle, Pwd);
noGui([User, Circle, Pwd, Status|StatusMsg]) ->
    StatusMsg2 = constructMsg(StatusMsg, []),
    noGui(User, Circle, Pwd, Status, StatusMsg2).

noGui(User, Circle, Pwd) ->
    application:set_env(eTodo, mode,   "noGui"),
    application:set_env(eTodo, user,   User),
    application:set_env(eTodo, circle, Circle),
    application:set_env(eTodo, pwd,    Pwd),
    gui().

noGui(User, Circle, Pwd, Status, StatusMsg) ->
    application:set_env(eTodo, mode,      "noGui"),
    application:set_env(eTodo, user,      User),
    application:set_env(eTodo, circle,    Circle),
    application:set_env(eTodo, pwd,       Pwd),
    application:set_env(eTodo, status,    Status),
    application:set_env(eTodo, statusMsg, StatusMsg),
    gui().

gui() ->
    application:load(eTodo),
    {_, Apps}   = application:get_key(eTodo, applications),
    Running     = application:which_applications(),
    RunningApps = [App || {App, _, _} <- Running],
    AppsToStart = Apps -- RunningApps,
    [application:start(App) || App <- AppsToStart],
    application:start(eTodo).

%%%===================================================================
%%% Internal functions
%%%===================================================================

constructMsg([], SoFar) ->
    lists:flatten(SoFar);
constructMsg([Word|Rest], []) ->
    constructMsg(Rest, [Word]);
constructMsg([Word|Rest], SoFar) ->
    constructMsg(Rest, [SoFar, " ", Word]).