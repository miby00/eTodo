%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eWeb).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/0,
         setTimerRef/2,
         setStatusUpdate/3,
         clearPage/0,
         appendToPage/1,
         loggedIn/1,
         loggedOut/1,
         webProxyCall/2,
         webProxyCall/5,
         removeSubscriber/1,
         getPort/0,
         link/3,
         listListsJSON/3,
         listTodos/3,
         listTodosJSON/3,
         createTodo/3,
         createTask/3,
         showTodo/3,
         show/3,
         index/3,
         showStatus/3,
         showLoggedWork/3,
         checkStatus/3,
         indexJSON/3,
         sendStatus/3,
         saveTodo/3,
         sendMsg/3,
         checkForMessage/3,
         message/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port         = 8099,
                messages     = [],
                subscribers  = [],
                lastMsg      = [],
                user         = "",
                guestUsers   = [],
                users        = [],
                headers      = [],
                timers       = [],
                status       = []}).

-include("eTodo.hrl").
-include_lib("inets/include/httpd.hrl").

-import(eTodoUtils, [makeRef/0, toDB/1, toDB/2, dateTime/0,
                     getRootDir/0, apply/4, default/2, addDateTime/2,
                     tryInt/1]).

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
start_link(User) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [User], []).

stop() ->
    gen_server:cast(?MODULE, stop).

getPort() ->
    gen_server:call(?MODULE, getPort).

appendToPage(Html) ->
    gen_server:cast(?MODULE, {appendToPage, Html}).

clearPage() ->
    gen_server:cast(?MODULE, clearPage).

loggedIn(User) ->
    gen_server:cast(?MODULE, {loggedIn, User}).

loggedOut(User) ->
    gen_server:cast(?MODULE, {loggedOut, User}).

removeSubscriber(Subscriber) ->
    gen_server:cast(?MODULE, {removeSubscriber, Subscriber}).

setTimerRef(User, TimerRef) ->
    gen_server:cast(?MODULE, {setTimerRef, User, TimerRef}).

setStatusUpdate(User, Status, StatusMsg) ->
    gen_server:cast(?MODULE, {setStatusUpdate, User, Status, StatusMsg}).

webProxyCall({checkStatus, _SessionId, _Env, _Input} = Message, Timeout) ->
    apply(gen_server, call, [?MODULE, Message, Timeout], "{\"timer\":0}");
webProxyCall({checkForMessage, _SessionId, _Env, _Input} = Message, Timeout) ->
    apply(gen_server, call, [?MODULE, Message, Timeout], "noMessages");
webProxyCall(Message, Timeout) ->
    apply(gen_server, call, [?MODULE, Message, Timeout], "").

link(SessionId, Env, Input) ->
    FileData = call({link, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, FileData).

listTodos(SessionId, Env, Input) ->
    HtmlPage = call({listsTodos, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

listTodosJSON(SessionId, Env, Input) ->
    HtmlPage = call({listTodosJSON, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

listListsJSON(SessionId, Env, Input) ->
    HtmlPage = call({listListsJSON, SessionId, Env, Input}),
    mod_esi:deliver(SessionId,HtmlPage).

createTodo(SessionId, Env, Input) ->
    HtmlPage = call({createTodo, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

createTask(SessionId, Env, Input) ->
    HtmlPage = call({createTask, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

showTodo(SessionId, Env, Input) ->
    HtmlPage = call({showTodo, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

show(SessionId, Env, Input) ->
    HtmlPage = call({show, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

message(SessionId, Env, Input) ->
    HtmlPage = call({message, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

index(SessionId, Env, Input) ->
    HtmlPage = call({index, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

showStatus(SessionId, Env, Input) ->
    HtmlPage = call({showStatus, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

showLoggedWork(SessionId, Env, Input) ->
    HtmlPage = call({showLoggedWork, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

checkStatus(SessionId, Env, Input) ->
    HtmlPage = call({checkStatus, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

indexJSON(SessionId, Env, Input) ->
    HtmlPage = call({indexJSON, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, HtmlPage).

sendStatus(SessionId, Env, Input) ->
    Status = call({sendStatus, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, Status).
saveTodo(SessionId, Env, Input) ->
    Status = call({saveTodo, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, Status).
sendMsg(SessionId, Env, Input) ->
    Status = call({sendMsg, SessionId, Env, Input}),
    mod_esi:deliver(SessionId, Status).

checkForMessage(SessionId, Env, Input) ->
    Result = call({checkForMessage, SessionId, Env, Input}, 10000),
    mod_esi:deliver(SessionId, Result).

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
init([User]) ->
    FileName = filename:join([getRootDir(), "www", "printout"]),
    filelib:ensure_dir(FileName),
    GuestUsers = getGuestUsers(User),
    Port = startWebServer(User, GuestUsers),
    {ok, #state{port       = Port,
                user       = User,
                guestUsers = GuestUsers}}.

getGuestUsers(User) ->
    %% Minimize guests to those users which the peer has shared todos with.
    #userCfg{ownerCfg = OwnerCfg} = eTodoDB:readUserCfg(User),
    Users  = (eTodoDB:getUsers() ++ default(OwnerCfg, [])) -- [User],
    Filter = fun(WUser) ->
                     case eTodoDB:getTodosSharedWith(User, WUser) of
                         [] ->
                             false;
                         _ ->
                             true
                     end
             end,
    lists:filter(Filter, Users).

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
handle_call(getPort, _From, State = #state{port = Port}) ->
    {reply, Port, State};

handle_call({call, Message, Timeout}, From,
             State = #state{headers = SessionHdrs}) ->
    SessionId = getSessionId(Message),
    Headers   = getHeaders(SessionId, State),
    State2    = State#state{headers = keepAliveSessions(SessionHdrs)},
    case proxyCall(Message, State2) of
        {false, State3 = #state{user = User}} ->
            case userOK(User, Message) of
                true ->
                    handle_call(Message, From, State3);
                false ->
                    {reply, Headers, State}
            end;
        {true, Pid, State3} ->
            Headers2 = getHeaders(SessionId, State3),
            spawn(?MODULE, webProxyCall,
                  [Pid, Headers2, Message, Timeout, From]),
            {noreply, State3};
        {true, Pid, Msg2, State3} ->
            Headers2 = getHeaders(SessionId, State3),
            spawn(?MODULE, webProxyCall,
                  [Pid, Headers2, Msg2, Timeout, From]),
            {noreply, State3}
    end;

handle_call({link, _SessionId, _Env, Input}, _From, State) ->
    Dict       = makeDict(Input),
    {ok, File} = find("filename",  Dict),
    {ok, Ref}  = find("reference", Dict),
    FileName   = filename:join([getRootDir(), "www", "linkedFiles",
                                Ref ++ "_" ++ File]),

    FileData =
        case file:read_file(FileName) of
            {ok, Bin} ->
                ["Content-Type: application/octet-stream\r\n"
                 "Content-Disposition: attachment; filename=", File,
                 "\r\n\r\n", zlib:gunzip(Bin)];
            Error ->
                Error
        end,
    {reply, FileData, State};

handle_call({listsTodos, _SessionId, _Env, Input}, _From,
            State = #state{user = User, status = SList}) ->
    Cfg = eTodo:getSearchCfg(),
    Flt = [?statusDone],
    Dict = makeDict(Input),
    {ok, List} = find("list",   Dict),
    {ok, Text} = find("search", Dict),

    HtmlPage   =
        case List of
            ?defLoggedWork ->
                doShowLoggedWork(User, Text);
            ?defShowStatus ->
                {Status, StatusMsg} = getStatus(User, SList),
                eHtml:showStatus(User, Status, StatusMsg);
            _ ->
                [eHtml:pageHeader(User),
                 eHtml:makeForm(User, List),
                 eHtml:makeTaskList(User, conv(List), Flt, Text, Cfg),
                 eHtml:pageFooter()]
        end,
    {reply, HtmlPage, State};
%%--------------------------------------------------------------------
%% JSON Stuff
%%--------------------------------------------------------------------

handle_call({listTodosJSON, _SessionId, _Env, Input}, _From,
            State = #state{user = User}) ->
    Cfg  = eTodo:getSearchCfg(),
    Flt  = [?statusDone],
    Dict = makeDict(Input),
    {ok, List} = find("list",   Dict),
    {ok, Text} = find("search", Dict),

    JSONData   = [eJSON:makeTodoList(User, conv(List), Flt, Text, Cfg)],
    {reply, ["Content-Type: application/x-javascript\r\n\r\n",JSONData], State};

handle_call({listListsJSON, _SessionId, _Env, Input}, _From,
            State = #state{user = User}) ->
    Dict = makeDict(Input),
    {ok, List} = find("list",   Dict),

    HtmlPage   = [eJSON:makeForm(User, List)],
    {reply, ["Content-Type: application/x-javascript\r\n\r\n",HtmlPage], State};

handle_call({createTodo, _SessionId, _Env, _Input}, _From,
            State = #state{user = User}) ->
    HtmlPage   = [eHtml:pageHeader(User),
                  eHtml:makeForm(User, ?defTaskList),
                  eHtml:createTaskForm(User),
                  eHtml:pageFooter()],
    {reply, HtmlPage, State};
handle_call({createTask, _SessionId, _Env, Input}, _From,
            State = #state{user = User}) ->
    Cfg  = eTodo:getSearchCfg(),
    Flt  = [?statusDone],
    Dict = makeDict(Input),

    {ok, TaskList} = find("list",     Dict),
    {ok, Status}   = find("status",   Dict),
    {ok, Prio}     = find("prio",     Dict),
    {ok, Desc}     = find("desc",     Dict),
    {ok, Comment}  = find("comment",  Dict),
    {ok, Progress} = find("progress", Dict),
    {ok, DueTime}  = find("dueTime",  Dict),

    Todo = #todo{uid         = makeRef(),
                 status      = toDB(Status),
                 priority    = toDB(Prio),
                 description = Desc,
                 comment     = Comment,
                 progress    = tryInt(Progress),
                 dueTime     = toDB(DueTime, time),
                 createTime  = dateTime()},

    Row = eTodoDB:getRow(User, TaskList),
    eTodoDB:addTodo(#userInfo{userName = User,
                              uid      = Todo#todo.uid,
                              row      = Row,
                              parent   = tryInt(TaskList)}, Todo),

    eTodo:todoCreated(TaskList, Row, Todo),
    HtmlPage   = [eHtml:pageHeader(User),
                  eHtml:makeForm(User, TaskList),
                  eHtml:makeTaskList(User, conv(TaskList), Flt, [], Cfg),
                  eHtml:pageFooter()],
    {reply, HtmlPage, State};
handle_call({showTodo, _SessionId, _Env, Input}, _From,
            State = #state{user = User}) ->
    Dict       = makeDict(Input),
    {ok, Uid}  = find("uid", Dict),
    Uid2       = binary_to_term(base64:decode(list_to_binary(Uid))),
    Todo       = eTodoDB:getTodo(Uid2),
    Columns    = eTodoDB:getColumns(User),
    ETodo      = eTodoUtils:makeETodo(Todo, User, Columns),
    HtmlPage   = [eHtml:pageHeader(User),
                  eHtml:makeForm(User, ?defTaskList),
                  eHtml:makeHtmlTaskCSS(ETodo),
                  eHtml:pageFooter()],
    {reply, HtmlPage, State};
handle_call({show, SessionId, Env, Input}, _From,
            State = #state{user    = User,
                           headers = SessionHdrList}) ->
    Headers    = getHeaders(SessionId, State),
    {_, WUser} = lists:keyfind(remote_user, 1, Env),
    Dict       = makeDict(Input),
    {ok, List} = find("list",      Dict),
    {ok, Text} = find("search",    Dict),
    {ok, Cfg}  = find("searchCfg", Dict),
    {ok, Flt}  = find("filter",    Dict),

    Todos      = eTodoDB:getTodosSharedWith(User, WUser),
    Columns    = eTodoDB:getColumns(User),
    SETodos    = [eTodoUtils:makeETodo(Todo, User, Columns) || Todo <- Todos],

    ETodos     = filterETodos(User, List, Text, Flt, Cfg, SETodos),

    HtmlPage   = [eHtml:pageHeader(User),
                  [eHtml:makeHtmlTaskCSS2(ETodo) || ETodo <- ETodos],
                  eHtml:pageFooter()],

    SessionHdrList2 = keepAliveSessions(SessionHdrList),
    {reply, Headers ++ HtmlPage, State#state{headers = SessionHdrList2}};
handle_call({message, _SessionId, _Env, _Input}, _From,
            State = #state{user        = User,
                           users       = Users,
                           messages    = Messages,
                           subscribers = Subs}) ->
    [gen_server:reply(From, "noMessages") || From <- Subs],
    TopMessages = top(Messages, 100),
    HtmlPage    = [eHtml:pageHeader(
                     "OnLoad=\"setTimeout('checkForMessage()', 10000);\"", User),
                   eHtml:makeForm(User, ?defTaskList),
                   "<div id=\"messageField\">" ++ TopMessages ++ "</div>",
                   eHtml:createSendMsg("All", lists:delete(User, Users)),
                   eHtml:pageFooter()],
    {reply, HtmlPage, State#state{messages = TopMessages, subscribers = []}};
handle_call({index, SessionId, _Env, _Input}, _From,
            State = #state{user    = User,
                           headers = SessionHdrList}) ->
    Headers = getHeaders(SessionId, State),
    Flt = [?statusDone],
    HtmlPage = [eHtml:pageHeader(User),
                eHtml:makeForm(User, ?defTaskList),
                eHtml:makeTaskList(User, ?defTaskList, Flt, [], undefined),
                eHtml:pageFooter()],

    SessionHdrList2 = keepAliveSessions(SessionHdrList),
    {reply, Headers ++ HtmlPage, State#state{headers = SessionHdrList2}};
handle_call({showStatus, SessionId, _Env, _Input}, _From,
            State = #state{user    = User,
                           headers = SessionHdrList,
                           status  = SList}) ->
    Headers = getHeaders(SessionId, State),
    {Status, StatusMsg} = getStatus(User, SList),

    HtmlPage = eHtml:showStatus(User, Status, StatusMsg),
    SessionHdrList2 = keepAliveSessions(SessionHdrList),
    {reply, Headers ++ HtmlPage, State#state{headers = SessionHdrList2}};
handle_call({checkStatus, SessionId, _Env, _Input}, _From,
            State = #state{user    = User,
                           headers = SessionHdrList,
                           timers  = Timers,
                           status  = StatusList}) ->
    Headers = getHeaders(SessionId, State),
    Timer    = lists:keyfind(User, 1, Timers),
    {Status, StatusMsg} = getStatus(User, StatusList),
    Seconds  = getSeconds(Timer),
    HtmlPage = "{\"timer\":" ++ integer_to_list(Seconds) ++ ","
        "\"status\":\"" ++ Status ++ "\",\"statusMsg\":\"" ++
        makeHtml(StatusMsg) ++ "\"}",
    SessionHdrList2 = keepAliveSessions(SessionHdrList),
    {reply, Headers ++ HtmlPage, State#state{headers = SessionHdrList2}};
handle_call({showLoggedWork, SessionId, _Env, Input}, _From,
            State = #state{user = User, headers = SessionHdrList}) ->
    Headers = getHeaders(SessionId, State),
    Dict    = makeDict(Input),
    {ok, Text} = find("search", Dict),
    HtmlPage = doShowLoggedWork(User, default(Text, "")),
    SessionHdrList2 = keepAliveSessions(SessionHdrList),
    {reply, Headers ++ HtmlPage, State#state{headers = SessionHdrList2}};
handle_call({indexJSON, _SessionId, _Env, _Input}, _From,
            State = #state{user = User}) ->
    Flt = [?statusDone],
    HtmlPage = [
                eJSON:makeTodoList(User, ?defTaskList, Flt, [], undefined)
               ],
    {reply, HtmlPage, State};
handle_call({sendStatus, _SessionId, _Env, Input}, _From,
            State = #state{user = User}) ->
    Dict = makeDict(Input),
    {ok, Status} = find("status", Dict),
    {ok, Uid}    = find("uid",    Dict),
    Todo1 = eTodoDB:getTodo(list_to_integer(Uid)),
    StatusDB = eTodoUtils:toDB(Status),
    DoneTime = eTodoUtils:doneTime(Todo1#todo.doneTime, StatusDB),
    Todo2 = Todo1#todo{status = StatusDB, doneTime = DoneTime},
    eTodoDB:updateTodo(User, Todo2),
    eTodo:todoUpdated(User, Todo2),
    {reply, "ok", State};
handle_call({saveTodo, _SessionId, _Env, Input}, _From,
            State = #state{user = User}) ->
    Dict = makeDict(Input),
    {ok, Status}      = find("status",   Dict),
    {ok, Uid}         = find("uid",      Dict),
    {ok, Description} = find("desc",     Dict),
    {ok, Comment}     = find("comment",  Dict),
    {ok, Progress}    = find("progress", Dict),
    {ok, DueTime}     = find("dueTime",  Dict),
    {ok, Priority}    = find("prio",     Dict),
    {ok, Lists}       = find("list",     Dict),

    Todo1    = eTodoDB:getTodo(list_to_integer(Uid)),
    StatusDB = eTodoUtils:toDB(Status),
    PrioDB   = eTodoUtils:toDB(Priority),
    DoneTime = eTodoUtils:doneTime(Todo1#todo.doneTime, StatusDB),
    Todo2 = Todo1#todo{status      = StatusDB,
                       doneTime    = DoneTime,
                       description = Description,
                       comment     = Comment,
                       progress    = tryInt(Progress),
                       priority    = PrioDB,
                       dueTime     = toDB(DueTime, time)},
    eTodoDB:updateTodo(User, Todo2),
    assignLists(User, Uid, Lists),
    eTodo:todoUpdated(User, Todo2),
    {reply, "ok", State};
handle_call({sendMsg, _SessionId, _Env, Input}, _From,
            State = #state{user = User, users = Users}) ->
    Dict = makeDict(Input),
    {ok, To}  = find("to",  Dict),
    {ok, Msg} = find("msg", Dict),
    UserList = case To of
                   "All" ->
                       lists:delete(User, Users);
                   Peer ->
                       [http_uri:decode(Peer)]
               end,
    Msg2 = http_uri:decode(Msg),
    ePeerEM:sendMsg(User, UserList, msgEntry, Msg2),
    eTodo:msgEntry(User, UserList, Msg2),
    {reply, "ok", State};

%% No messages to send to web client
handle_call({checkForMessage, _SessionId, _Env, _Input}, From,
    State = #state{subscribers = Subscribers, messages = []}) ->
    %% Remove subscriber after 10 secs.
    timer:apply_after(10000, ?MODULE, removeSubscriber, [From]),
    {noreply, State#state{subscribers = [From|Subscribers]}};
%% New messages to be sent to web client.
handle_call({checkForMessage, SessionId, _Env, _Input}, From,
    State = #state{messages    = [Html|Messages],
                   subscribers = Subscribers,
                   lastMsg     = LastMsg}) ->
    LastMsg2 = keepAliveSessions(LastMsg),
    case lists:keytake(SessionId, 1, LastMsg2) of
        {value, {SessionId, Html}, _LastMsg} ->
            %% Remove subscriber after 10 secs.
            timer:apply_after(10000, ?MODULE, removeSubscriber, [From]),
            {noreply, State#state{subscribers = [From|Subscribers],
                                  lastMsg     = LastMsg2}};
        {value, {SessionId, _Html}, LastMsg3} ->
            LastMsg4 = [{SessionId, Html} | LastMsg3],
            {reply, [Html|Messages], State#state{lastMsg = LastMsg4}};
        false ->
            LastMsg5 = [{SessionId, Html} | LastMsg2],
            {reply, [Html|Messages], State#state{lastMsg = LastMsg5}}
    end;

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
handle_cast({appendToPage, Html}, State = #state{subscribers = [],
                                                 messages    = Messages}) ->
    {noreply, State#state{messages = [flattenMsg(Html)|Messages]}};
handle_cast({appendToPage, Html}, State = #state{subscribers = Subs,
                                                 messages    = Messages}) ->
    FlatMsg   = flattenMsg(Html),
    Messages2 = [FlatMsg|Messages],
    [gen_server:reply(From, Messages2) || From <- Subs],
    {noreply, State#state{messages    = Messages2,
                          lastMsg     = FlatMsg,
                          subscribers = []}};
handle_cast(clearPage, State) ->
    {noreply, State#state{messages = [], subscribers = [], lastMsg = []}};
handle_cast({loggedIn, User}, State = #state{users = Users}) ->
    {noreply, State#state{users = [User|Users]}};
handle_cast({loggedOut, User}, State = #state{users = Users}) ->
    {noreply, State#state{users = lists:delete(User, Users)}};
handle_cast({removeSubscriber, Subscriber},
            State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Subscriber, Subs)}};
handle_cast({setStatusUpdate, User, Status, StatusMsg},
            State = #state{status = StatusList}) ->
    StatusList2 = lists:keystore(User, 1, StatusList, {User, Status, StatusMsg}),
    {noreply, State#state{status = StatusList2}};
handle_cast({setTimerRef, User, TimerRef}, State = #state{timers = Timers}) ->
    Timers2 = lists:keystore(User, 1, Timers, {User, TimerRef}),
    {noreply, State#state{timers = Timers2}};
handle_cast(stop, State) ->
    {stop, normal, State};
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
terminate(_Reason, #state{port = Port, user = User, guestUsers = Users}) ->
    delUsers([User|Users], Port),
    inets:stop(),
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
getSeconds({_, undefined}) ->
    0;
getSeconds(false) ->
    0;
getSeconds({_, Ref}) ->
    case erlang:read_timer(Ref) of
        false ->
            0;
        MSecs ->
            round(MSecs / 1000)
    end.

addUsers([], _CPwd, _Port) ->
    ok;
addUsers([User|Rest], CPwd, Port) ->
    mod_auth:add_user(User, CPwd, "User", Port, "/eTodo"),
    mod_auth:add_group_member("users", User, Port, "/eTodo"),
    addUsers(Rest, CPwd, Port).

delUsers([], _Port) ->
    ok;
delUsers([User|Rest], Port) ->
    mod_auth:delete_user(User, Port, "/eTodo"),
    mod_auth:delete_group_member("users", User, Port, "/eTodo"),
    delUsers(Rest, Port).

startWebServer(User, GuestUsers) ->
    #userCfg{webEnabled  = Enabled,
             webPort     = WPort,
             webPassword = WPwd} = eTodoDB:readUserCfg(User),
    case Enabled of
        true ->
            application:start(inets),
            Port = doStartWebServer(WPort, WPort + 10),
            mod_auth:add_user(User, WPwd, "Super User", Port, "/eTodo"),
            mod_auth:add_group_member("users", User, Port, "/eTodo"),
            CPwd  = ePeerCircle:getPwd(),
            addUsers(GuestUsers, CPwd, Port),
            PortStr = integer_to_list(Port),
            ConCfg = default(eTodoDB:getConnection(User),
                             #conCfg{host = "localhost"}),
            Host   = default(ConCfg#conCfg.host, "localhost"),
            ePeerEM:sendMsg(system,  [User], systemEntry,
                            "https://"++ Host ++ ":" ++ PortStr ++ "/eTodo"
                            "/eWeb:index shows your todos in a browser."),
            ePeerEM:sendMsg(system,  [User], systemEntry,
                            "https://"++ Host ++ ":" ++ PortStr ++ "/mobile.html"
                            " for an alternative web gui for the phone."),
            Port;
        _ ->
            -1
    end.

doStartWebServer(Port, Port) ->
    -1;
doStartWebServer(Port, MaxPort) ->
    Root     = getRootDir(),
    SrvRoot  = case application:get_env(eLog, logDir) of
                   undefined ->
                       filename:join([Root, "logs"]);
                   {ok, LogFileName} ->
                       filename:dirname(LogFileName)
               end,
    DocRoot  = filename:join([Root, "www"]),
    CertFile = filename:join([Root, "localhost.pem"]),
    filelib:ensure_dir(SrvRoot),
    filelib:ensure_dir(DocRoot),
    Result = inets:start(httpd, [{modules, [mod_alias,
                                            mod_auth,
                                            mod_esi,
                                            mod_actions,
                                            mod_get,
                                            mod_head,
                                            mod_log,
                                            mod_disk_log]},
                                 {port, Port},
                                 {server_name,   "eTodo"},
                                 {server_root,   SrvRoot},
                                 {document_root, DocRoot},
                                 {directory,
                                  {"/eTodo", [
                                              {auth_name, "eTodo Server"},
                                              {allow_from, all},
                                              {auth_type, mnesia},
                                              {require_group, ["users"]}
                                             ]}},
                                 {socket_type, {essl, [{certfile, CertFile}]}},
                                 {ipfamily, inet},
                                 {erl_script_alias, {"/eTodo", [eWeb]}},
                                 {error_log,    "error.log"},
                                 {security_log, "security.log"},
                                 {transfer_log, "transfer.log"},
                                 {mime_types,[
                                              {"html","text/html;charset=utf-8"},
                                              {"css","text/css"},
                                              {"js","application/x-javascript"}
                                             ]}]),
    case Result of
        {ok, _Pid} ->
            Port;
        _ ->
            eLog:log(error, ?MODULE, doStartWebServer, [Port, Result],
                     "Failed to start web server at port", ?LINE),
            %% Failed to start at port, try next.
            doStartWebServer(Port + 1, MaxPort)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deliver top Num messages.
%%
%% @spec top(Messages, Num) -> TopNumMessages
%% @end
%%--------------------------------------------------------------------
top(Messages, Num) ->
    top(Messages, Num, []).

top([], _Num, Acc) ->
    lists:reverse(Acc);
top(_Messages, 0, Acc) ->
    Acc;
top([Message|Rest], Num, Acc) ->
    top(Rest, Num - 1, [Message|Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call eWeb with arguments that allow us to look if we should make
%% a local or remote call.
%%
%% @spec call({Msg, SessionId, Input}) -> HtmlPage
%% @end
%%--------------------------------------------------------------------
call(Message) -> call(Message, 5000).

call({checkForMessage, _SessionId, _Env, _Input} = Message, Timeout) ->
    apply(gen_server, call,
          [?MODULE, {call, Message, Timeout + 1000}, Timeout], "noMessages");
call(Message, Timeout) ->
    apply(gen_server, call,
          [?MODULE, {call, Message, Timeout + 1000}, Timeout], "").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Lookup if we should call a proxy or not.
%%
%% @spec proxyCall({Msg, SessionId, Input}, State) -> {false, State2} |
%%                                                    {true, Pid}
%% @end
%%--------------------------------------------------------------------
proxyCall({Message, SessionId, Env, Input},
          State = #state{headers = SessionHdrList})
  when (Message == index)      or
       (Message == show)       or
       (Message == showStatus) or
       (Message == showLoggedWork) ->
    Dict = makeDict(Input),
    case find("proxy", Dict) of
        {ok, User} when is_list(User) ->
            case getPeer(User) of
                undefined ->
                    Headers1 = removeCookieIfPresent("eWebProxy", Env),
                    Headers2 = removeCookieIfPresent("eWebToken", Env, Headers1),
                    SessionHdrList2 =
                        lists:keystore(SessionId, 1,
                                       SessionHdrList,
                                       {SessionId, Headers2}),
                    {false, State#state{headers = SessionHdrList2}};
                Pid ->
                    Headers =
                        case find("token", Dict) of
                            {ok, Token} when is_list(Token) ->
                                Headers2 = setCookie("eWebProxy", User),
                                setCookie("eWebToken", Token, Headers2);
                            _ ->
                                setCookie("eWebProxy", User)
                        end,
                    SessionHdrList2 =
                        lists:keystore(SessionId, 1,
                                       SessionHdrList,
                                       {SessionId, Headers}),
                    {true, Pid, State#state{headers = SessionHdrList2}}
            end;
        _ ->
            Headers1 = removeCookieIfPresent("eWebToken", Env),
            Headers2 = removeCookieIfPresent("eWebProxy", Env, Headers1),

            SessionHdrList2 =
                lists:keystore(SessionId, 1,
                               SessionHdrList,
                               {SessionId, Headers2}),
            {false, State#state{headers = SessionHdrList2}}
    end;
proxyCall(Msg = {_Msg, SessionId, Env, _Input},
          State = #state{headers = SessionHdrList}) ->
    case getCookie("eWebProxy", Env) of
        undefined ->
            SessionHdrList2 = lists:keydelete(SessionId, 1, SessionHdrList),
            {false, State#state{headers = SessionHdrList2}};
        User ->
            case getPeer(User) of
                undefined ->
                    Headers1 = removeCookie("eWebToken"),
                    Headers2 = removeCookie("eWebProxy", Headers1),

                    SessionHdrList2 =
                        lists:keystore(SessionId, 1,
                                       SessionHdrList,
                                       {SessionId, Headers2}),
                    {false, State#state{headers = SessionHdrList2}};
                Pid ->
                    Token = getCookie("eWebToken", Env),
                    Headers =
                        case Token of
                            undefined ->
                                setCookie("eWebProxy", User);
                            Token ->
                                Headers2  = setCookie("eWebToken", Token),
                                setCookie("eWebProxy", User, Headers2)
                        end,
                    Msg2 = updateMsg(Msg, Token),
                    SessionHdrList2 =
                        lists:keystore(SessionId, 1,
                                       SessionHdrList,
                                       {SessionId, Headers}),
                    {true, Pid, Msg2, State#state{headers = SessionHdrList2}}
            end
    end.

getPeer(undefined) ->
    undefined;
getPeer(User) ->
    case ePeerServer:getPeers([User]) of
        [] ->
            undefined;
        [Pid] ->
            Pid
    end.

updateMsg(Msg, undefined) ->
    Msg;
updateMsg({Name, SessionId, Env, ""}, Token) ->
    {Name, SessionId, Env, "token=" ++ http_uri:encode(Token)};
updateMsg({Name, SessionId, Env, Input}, Token) ->
    {Name, SessionId, Env, Input ++ "&token=" ++ http_uri:encode(Token)}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert List to correct type, string or integer.
%%
%% @spec conv(List) -> List | SubTodo
%% @end
%%--------------------------------------------------------------------
conv(List) ->
    case catch list_to_integer(List) of
        {'EXIT', _} ->
            List;
        SubTodo ->
            SubTodo
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make dictonary out of args.
%%
%% @spec makeDict(Input) -> Dict.
%% @end
%%--------------------------------------------------------------------
makeDict(Input) ->
    KeyValueList = httpd:parse_query(Input),
    dict:from_list(KeyValueList).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get value from dictionary and convert value to iso-8859-1
%%
%% @spec find(Key, Dict) -> Result
%% @end
%%--------------------------------------------------------------------
find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            BinValue = list_to_binary(Value),
            Bin = unicode:characters_to_binary(BinValue, utf8, latin1),
            {ok, binary_to_list(Bin)};
        _ ->
            {ok, undefined}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make call through proxy.
%%
%% @spec webProxyCall(Pid, Message, Timeout, From) -> HtmlPage.
%% @end
%%--------------------------------------------------------------------
webProxyCall(Pid, Headers, Message, Timeout, From) ->
    HtmlPage = apply(ePeer, webProxyCall, [Pid, Message, Timeout], ""),
    gen_server:reply(From, Headers ++ HtmlPage).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Flatten html message.
%%
%% @spec flattenMsg(HtmlIOList) -> HtmlString.
%% @end
%%--------------------------------------------------------------------
flattenMsg(HtmlIOList) ->
    lists:flatten(io_lib:format("~s", [HtmlIOList])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if user is ok.
%%
%% @spec userOK(User, Message) -> true | false.
%% @end
%%--------------------------------------------------------------------
userOK(_User, {show, _SessionId, _Env, _Input}) ->
    true;
userOK(_User, {link, _SessionId, _Env, _Input}) ->
    true;
userOK(User, {_Msg, _SessionId, Env, _Input}) ->
    case lists:keyfind(remote_user, 1, Env) of
        {_, User} ->
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update show view according to input.
%%
%% @spec filterETodos(User, List, SearchTxt, Flt, Cfg, SharedETodos) -> ETodos
%% @end
%%--------------------------------------------------------------------
filterETodos(User, List, Text, Flt, Cfg, ETodos) ->
    SearchConfig = case string:tokens(default(Cfg, ""), ";") of
                       [] ->
                           eTodo:getSearchCfg();
                       SCfg ->
                           makeAtomList(SCfg)
                   end,
    FilterConfig = case string:tokens(default(Flt, ""), ";") of
                       [] ->
                           [];
                       Filter ->
                           makeIntegerList(Filter)
                   end,
    View = eTodoDB:getETodos(User, tryInt(default(List, ?defTaskList)),
                             FilterConfig, default(Text, ""), SearchConfig),
    lists:filter(fun(ETodo) -> lists:member(ETodo, ETodos) end, View).

makeAtomList(List) ->
    Fun = fun(Element) ->
                  list_to_atom(Element)
          end,
    lists:map(Fun, List).

makeIntegerList(List) ->
    Fun = fun(Element) ->
                  list_to_integer(Element)
          end,
    lists:map(Fun, List).

assignLists(_User, _Uid, undefined) ->
    ok;
assignLists(User, Uid, Lists) ->
    ETodoLists = [tryInt(Value) || Value <- string:tokens(Lists, ";")],
    eTodoDB:assignLists(User, tryInt(Uid), ETodoLists).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get cookie
%%
%% @spec getCookie(Cookie, Env) -> Value | undefined
%% @end
%%--------------------------------------------------------------------
getCookie(Cookie, Env) ->
    case lists:keyfind(http_cookie, 1, Env) of
        {http_cookie, Cookies} ->
            CList   = string:tokens(Cookies, "; "),
            CKVList = [splitOnEqual(KVVal) || KVVal <- CList],
            case lists:keyfind(Cookie, 1, CKVList) of
                {Cookie, Value} ->
                    Value;
                false ->
                    undefined
            end;
        false ->
            undefined
    end.

splitOnEqual(Value) ->
    splitOnEqual(Value, []).

splitOnEqual("", SoFar) ->
    {lists:reverse(SoFar), ""};
splitOnEqual("=" ++ Rest, SoFar) ->
    {lists:reverse(SoFar), Rest};
splitOnEqual([Char|Rest], SoFar) ->
    splitOnEqual(Rest, [Char|SoFar]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set cookie
%%
%% @spec setCookie(Cookie, Value) -> Header
%% @end
%%--------------------------------------------------------------------
setCookie(Cookie, Value) ->
    EDTime   = addDateTime(dateTime(), {{0,0,7}, {0,0,0}}),
    ExpDTime = httpd_util:rfc1123_date(EDTime),
    "Set-Cookie: " ++ Cookie ++ "=" ++ Value ++
        "; Expires=" ++ ExpDTime ++ "; httpOnly\r\n\r\n".
setCookie(Cookie, Value, Header) ->
    EDTime   = addDateTime(dateTime(), {{0,0,7}, {0,0,0}}),
    ExpDTime = httpd_util:rfc1123_date(EDTime),
    "Set-Cookie: " ++ Cookie ++ "=" ++ Value ++
        "; Expires=" ++ ExpDTime ++ "; httpOnly\r\n"
        ++ Header.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove cookie
%%
%% @spec removeCookie(Cookie) -> Header
%% @end
%%--------------------------------------------------------------------
removeCookie(Cookie) ->
    %% The cookie should be removed, set expire to now.
    ExpDTime = httpd_util:rfc1123_date(dateTime()),
    "Set-Cookie: " ++ Cookie ++ "=\"\";"
        "Expires=" ++ ExpDTime ++ "; httpOnly\r\n\r\n".

removeCookie(Cookie, Header) ->
    %% The cookie should be removed, set expire to now.
    ExpDTime = httpd_util:rfc1123_date(dateTime()),
    "Set-Cookie: " ++ Cookie ++ "=\"\";"
        "Expires=" ++ ExpDTime ++ "; httpOnly\r\n"
        ++ Header.

removeCookieIfPresent(Cookie, Env) ->
    case getCookie(Cookie, Env) of
        undefined ->
            "";
        _Value ->
            removeCookie(Cookie)
    end.

removeCookieIfPresent(Cookie, Env, Header) ->
    case getCookie(Cookie, Env) of
        undefined ->
            "";
        _Value ->
            removeCookie(Cookie, Header)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get status and status msg for show timer function.
%%
%% @spec getStatus(User, SList) -> {Status, StatusMsg}
%% @end
%%--------------------------------------------------------------------
getStatus(User, SList) ->
    case lists:keyfind(User, 1, SList) of
        false ->
            {"", ""};
        {User, Status, StatusMsg} ->
            {Status, StatusMsg}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make Html text of string.
%% @spec makeHtml(StatusMsg) -> HtmlText
%% @end
%%--------------------------------------------------------------------
makeHtml(StatusMsg) ->
    binary_to_list(eHtml:makeHtml(StatusMsg)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Construct logged work html page.
%% @spec doShowLoggedWork(User) -> Html
%% @end
%%--------------------------------------------------------------------
doShowLoggedWork(User, Text) ->
    Date    = checkInputForDate(Text),
    CalDays = calendar:date_to_gregorian_days(Date),
    CalDate = calendar:gregorian_days_to_date(CalDays),
    [eHtml:pageHeader(User),
     eHtml:makeForm(User, ?defTaskList),
     eHtml:showLoggedWork(User, CalDate),
     eHtml:pageFooter()].

checkInputForDate(Text) ->
    Numbers = lists:filter(fun(Char) ->
                                   if (Char >= $0) and (Char =< $9) ->
                                           true;
                                      true ->
                                           false
                                   end
                           end, Text),

    case catch makeDate(Numbers) of
        {'EXIT', _} -> %% Default to 6 days ago.
            calendar:gregorian_days_to_date(
              calendar:date_to_gregorian_days(date()) - 6);
        Date ->
            Date
    end.

makeDate(Numbers) ->
    Year  = string:sub_string(Numbers, 1, 4),
    Month = string:sub_string(Numbers, 5, 6),
    Day   = string:sub_string(Numbers, 7, 8),
    {list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)}.

keepAliveSessions(SessionTuple) ->
    lists:filter(fun ({Pid, _}) -> is_process_alive(Pid) end, SessionTuple).

getSessionId({_Message, SessionId, _Env, _Input}) ->
    SessionId;
getSessionId(_) ->
    undefined.

getHeaders(SessionId, #state{headers = SessionHdrList}) ->
    case lists:keyfind(SessionId, 1, SessionHdrList) of
        false ->
            [];
        {SessionId, Headers} ->
            Headers
    end.
