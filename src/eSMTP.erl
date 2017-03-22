%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 16. Mar 2017 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eSMTP).
-author("mikael.bylund@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/0,
         updateConfig/0,
         setUser/1,
         sendMail/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {host, port, auth, smtpUser, smtpPwd,
                active = false, user, localHost}).

-define(SSL_TLS_Port,  465).
-define(SMTP_Port,      25).

-include_lib("eTodo/include/eTodo.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

updateConfig() ->
    gen_server:cast(?SERVER, updateConfig).

setUser(User) ->
    gen_server:cast(?SERVER, {setUser, User}).

sendMail(From, To, Msg) ->
    gen_server:cast(?SERVER, {sendMail, From, To, Msg}).

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
-spec(init(Args :: term()) ->
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).
init([]) ->
    updateConfig(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(updateConfig, State = #state{user = undefined}) ->
    {noreply, State};
handle_cast(updateConfig, State) ->
    UserCfg = eTodoDB:readUserCfg(State#state.user),
    ConCfg  = eTodoDB:getConnection(State#state.user),
    {noreply, State#state{host      = UserCfg#userCfg.smtpServer,
                          port      = UserCfg#userCfg.smtpPort,
                          smtpUser  = UserCfg#userCfg.smtpUser,
                          smtpPwd   = UserCfg#userCfg.smtpPwd,
                          auth      = UserCfg#userCfg.smtpAuth,
                          active    = UserCfg#userCfg.smtpEnabled,
                          localHost = ConCfg#conCfg.host}};
handle_cast({setUser, User}, State) ->
    {noreply, State#state{user = User}};
handle_cast(_Msg, State = #state{active = false}) ->
    {noreply, State};
handle_cast({sendMail, From, To, Msg}, State) ->
    doSendMail(From, To, Msg, State),
    {noreply, State};
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

doSendMail(From, To, Msg, State = #state{auth = Auth,
                                         host = Host,
                                         port = Port}) ->
    try
        {Socket, Type} = connect(Host, Port, Auth),
        smtpTransaction(Socket, Type, From, To, State),
        transferMail(Socket, Type, Msg)
    catch
        throw:{connectFailed, Reason} -> Reason
    end.

connect(Host, Port, "SSL/TLS") ->
    doConnect(Host, Port, ssl);
connect(Host, Port, _Auth) ->
    doConnect(Host, Port, gen_tcp).

doConnect(Host, Port, Type) ->
    case Type:connect(Host, Port, [{packet,    raw},
                                     {reuseaddr, true},
                                     {active,    false},
                                     {nodelay,   true}, binary]) of
        {ok, Socket} ->
            {Socket, Type};
        Reason ->
            throw({connectFailed, Reason})
    end.

smtpTransaction(Socket, Type, From, To, State = #state{auth = "None"}) ->
    sendAndLog(Socket, Type, ["HELO ", State#state.localHost, "\r\n"]),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, ["MAIL FROM: <", From, ">\r\n"]),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, ["RCPT TO: <", To, ">\r\n"]),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, "DATA\r\n"),
    checkResponse(Socket, Type, doRecvLine(Socket, Type));
smtpTransaction(Socket, Type, From, To, State) ->
    sendAndLog(Socket, Type, ["EHLO ", State#state.localHost, "\r\n"]),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    doAuthentication(Socket, Type, State),
    sendAndLog(Socket, Type, ["MAIL FROM: <", From, ">\r\n"]),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, ["RCPT TO: <", To, ">\r\n"]),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, "DATA\r\n"),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Read one line from smtp server response.
%%
%% @spec doRecvLine(Sock) -> Line
%% @end
%%--------------------------------------------------------------------
doRecvLine(Sock, Type) ->
    doRecvLine(Sock , Type, <<>>).

doRecvLine(Sock, Type, Line) ->
    case Type:recv(Sock, 1, 5000) of
        {ok, <<"\n">>} ->
            <<Line/binary, "\n">>;
        {ok, Char} ->
            doRecvLine(Sock, Type, <<Line/binary, Char/binary>>);
        {error, closed} ->
            (catch Type:close(Sock)),
            throw({smtpError, {error, closed}});
        {error, Reason} ->
            throw({smtpError, {error, Reason}})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check response code
%%
%% @spec checkResponse
%% @end
%%--------------------------------------------------------------------
checkResponse(Sock, Type, Response) ->
    Result = check(Sock, Type, Response, <<>>),
    eLog:log(debug, ?MODULE, sendAndLog, [Sock, Type, Result],
             "Receiving smtp response.", ?LINE),
    Result.

check(_Sock, _Type, Response, Acc) when byte_size(Response) < 4 ->
    throw({smtpError, {error, <<Acc/binary, Response/binary>>}});
check(Sock, Type, CResp = <<Response:4/bytes, _Rest/binary>>, Acc) ->
    <<Code:3/bytes, Rest/binary>> = Response,
    case {Code, Rest} of
        {Code, <<"-">>} ->
            %% Read next line of response
            check(Sock, Type,
                  doRecvLine(Sock, Type),
                  <<Acc/binary, CResp/binary>>);

        {<<"220">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"221">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"235">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"250">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"251">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"252">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"334">>, _} -> <<Acc/binary, CResp/binary>>;
        {<<"354">>, _} -> <<Acc/binary, CResp/binary>>;

        _ ->
            throw({smtpError, {error, <<Acc/binary, CResp/binary>>}})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% sendAndLog
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
sendAndLog(Socket, Type, Data) ->
    eLog:log(debug, ?MODULE, sendAndLog, [Socket, Type, Data],
             "Sending smtp message.", ?LINE),
    Type:send(Socket, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transfer email
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
transferMail(Socket, Type, Msg) ->
    sendAndLog(Socket, Type, Msg),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Authenticate SMTP connection
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
doAuthentication(Socket, Type, #state{smtpUser = User, smtpPwd = Pwd}) ->
    sendAndLog(Socket, Type, "AUTH LOGIN\r\n"),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, base64:encode_to_string(User)++"\r\n"),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)),
    sendAndLog(Socket, Type, base64:encode_to_string(Pwd)++"\r\n"),
    checkResponse(Socket, Type, doRecvLine(Socket, Type)).