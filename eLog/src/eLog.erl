%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Mikael Bylund
%%%-------------------------------------------------------------------
-module(eLog).

-define(Type, file).

-export([log/6, log/7]).

log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber, undefined).

log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber, ProtModule) ->
    case ?Type of
	io ->
	    io:format("~nLog time: ~p~nModule: ~p; Function: ~p; "
		      "Linenumber: ~p~n" ++ ErrorDesc ++ "~nArgs: ~p~n", 
		      [time(), Module, Function, LineNumber , [Args]]);	
	file ->
	    (catch eLogWriter:log(LogLevel, Module, Function, 
				  Args, ErrorDesc, LineNumber));
	info ->
	    (catch ProtModule:info(LogLevel, Module, Function, 
				   Args, ErrorDesc, LineNumber));
        none ->
	    ok
    end.

