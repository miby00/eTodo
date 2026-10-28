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
    resolveEnv(),
    case eTodoSup:start_link() of
	{ok, Pid} ->
		case application:get_env(eTodo, mode) of
			{ok, "noGui"} ->
                eTodoNoGui:start_link();
			_ ->
                eTodo:start(detached)
		end,
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

%% A sys.config string written as "%NAME%" is replaced by the value of the
%% environment variable NAME, to keep secrets out of the file.
resolveEnv() ->
    [application:set_env(eTodo, Key, resolveValue(Key, Value))
     || {Key, Value} <- application:get_all_env(eTodo)],
    ok.

resolveValue(Key, [$%|_] = Value) ->
    case lists:last(Value) =:= $% andalso length(Value) > 2 of
        true  -> lookupEnv(Key, lists:sublist(Value, 2, length(Value) - 2),
                           Value);
        false -> Value
    end;
resolveValue(Key, Values) when is_list(Values) ->
    [resolveValue(Key, Value) || Value <- Values];
resolveValue(Key, Value) when is_tuple(Value) ->
    list_to_tuple(resolveValue(Key, tuple_to_list(Value)));
resolveValue(_Key, Value) ->
    Value.

lookupEnv(Key, Name, Original) ->
    case lists:all(fun(C) -> C =:= $_ orelse (C >= $A andalso C =< $Z) orelse
                             (C >= $a andalso C =< $z) orelse
                             (C >= $0 andalso C =< $9) end, Name)
         andalso os:getenv(Name) of
        Value when is_list(Value) -> Value;
        false ->
            logger:warning("eTodo: ~p refers to unset environment "
                           "variable ~s", [Key, Name]),
            Original;
        _ -> Original
    end.
