%%%-------------------------------------------------------------------
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @copyright (C) 2012, Anders Ramsell
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%%-------------------------------------------------------------------
-module(eLogWriter).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 stop/0,
	 log/6
	]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {parent,
		logHandle}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> void()
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?MODULE, stop). 

%%--------------------------------------------------------------------
%% @doc
%% Sends a log message to the server
%%
%% @spec log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) -> void()
%% @end
%%--------------------------------------------------------------------
log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    gen_server:cast(?MODULE, 
		    {log,
		     LogLevel, Module, Function, Args, ErrorDesc, LineNumber}). 

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
    Handle = openLog(),
    {ok, #state{parent    = Parent,
		logHandle = Handle}}.

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
handle_call(_Request, _From, State) ->
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
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({log,
	     LogLevel, Module, Function, Args, ErrorDesc, LineNumber},
	    State = #state{logHandle = Handle}) ->
    LogString = io_lib:format("~nLevel: ~p; Log time: ~p~nModule: ~p; "
			      "Function: ~p; Linenumber: ~p~n~80p~n"
			      "Args: ~80p~n",
			      [LogLevel, time(), Module, Function, LineNumber,
			       ErrorDesc, Args]),
    disk_log:blog(Handle, LogString),
    {noreply, State};
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


%%======================================================================
%% Function : openLog(RootDir) -> Handle
%% Purpose  : Open a log.
%% Types    : Handle = term()
%%----------------------------------------------------------------------
%% Notes    : 
%%======================================================================
openLog() ->
    MaxNoBytes = 2 * 1024 * 1024,
    MaxNoFiles = 10,
    PrivDir    = code:priv_dir("eLog"),
    FileName   = filename:join([PrivDir, "Logs", "eLog"]),
    Args       = [{name,   eLogWriter},
		  {file,   FileName},
		  {linkto, self()},
		  {repair, true},
		  {type,   wrap},
		  {format, external},
		  {size,   {MaxNoBytes, MaxNoFiles}}],
    ensureDir(FileName),
    case disk_log:open(Args) of
	{ok, Handle} ->
	    Handle;
	{repaired, Handle, _, _} ->
	    Handle;
	{error, Reason} ->
	    io:format("Failed to open log: ~p~n", [{error, Reason}]),
	    undefined
    end.

ensureDir(FileName) ->
    case filelib:ensure_dir(FileName) of
	ok ->
	    ok; 
	{error, Reason} ->
	    erlang:error(Reason)
    end.
