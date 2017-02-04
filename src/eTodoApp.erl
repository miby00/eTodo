%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 8 Aug 2012 by Mikael Bylund
%%%-------------------------------------------------------------------
-module(eTodoApp).
-author("mikael.bylund@gmail.com").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    case eTodoSup:start_link() of
	{ok, Pid} -> 
	    eTodo:start(detached),
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    eTodo:stop(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
