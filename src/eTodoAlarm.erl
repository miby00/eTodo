%%%-------------------------------------------------------------------
%%% @author mikael <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, mikael
%%% @doc
%%%
%%% @end
%%% Created :  8 Jul  2012 by mikael <mikael.bylund@gmail.com>
%%% Changed :  6 sept 2012 by gusv   <gunnar@sverredal.se>
%%%           24 Mars 2017 by mikael <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eTodoAlarm).

-behaviour(gen_server).

%% API
-export([start_link/0, runCmd/1, loggedIn/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eTodo/include/eTodo.hrl").

-define(SERVER, ?MODULE).

-record(state, {user}).

-import(eTodoUtils, [default/2, dateTime/0, addDateTime/2]).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

loggedIn(User) ->
    gen_server:cast(?SERVER, {loggedIn, User}).

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
    {ok, #state{}, 0}.

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
    Reply = ok,
    {reply, Reply, State, 30000}.

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
handle_cast({loggedIn, User}, State) ->
    {noreply, State#state{user = User}, 30000};
handle_cast(_Msg, State) ->
    {noreply, State, 30000}.

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
handle_info(timeout, State = #state{user = User}) ->
    checkAlarms(User),
    {noreply, State, 30000}.

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

checkAlarms(undefined) ->
    ok;
checkAlarms(User) ->
    AlarmCfg = eTodoDB:getReminders(User),
    checkAlarms2(AlarmCfg, dateTime() ).

checkAlarms2([], _Now) ->
    ok;
checkAlarms2([Alarm| Rest], Now) ->
    checkAlarm(Alarm, Now),
    checkAlarms2(Rest, Now).

checkAlarm(#alarmCfg{startDate = undefined}, _Now) ->
    ok;
checkAlarm(Alarm = #alarmCfg{nextAlarm = undefined}, _Now) ->
    %% This is a new alarm.
    eTodoDB:addReminder( initAlarm(Alarm) );
checkAlarm(#alarmCfg{nextAlarm = never}, _Now) ->
    ok;
checkAlarm(#alarmCfg{nextAlarm = Next}, Now) when (Next > Now) ->
    ok;
checkAlarm(AlarmCfg = #alarmCfg{uid = Uid, execCmd = Cmd}, Now) ->
    Todo = default(eTodoDB:getTodo(Uid), #todo{status = deleted}),
    case (Todo#todo.status =/= done) and (Todo#todo.status =/= deleted) of
        true ->
            ePeerEM:alarmEntry(Uid, Todo#todo.description),
            spawn(?MODULE, runCmd, [Cmd]),
            sendEmail(AlarmCfg, Todo#todo.description),
            AlarmCfg2 = setNext(AlarmCfg, Now),
            eTodoDB:addReminder(AlarmCfg2#alarmCfg{lastAlarm = Now});
        false ->
            %% Remove reminder, task finished or deleted.
            eTodoDB:delReminder(AlarmCfg)
    end.

runCmd(undefined) ->
    ok;
runCmd(Cmd) ->
    os:cmd(Cmd).

sendEmail(#alarmCfg{emailReminder = false}, _Text) ->
    ok;
sendEmail(#alarmCfg{emailReminder = true, userName = User, uid = Uid}, Text) ->
    ConCfg = eTodoDB:getConnection(User),
    case ConCfg#conCfg.email of
        undefined ->
            ok;
        EmailAddr ->
            {_, Msg} = eHtml:generateAlarmMsg(Uid, Text),
            Msg2     = iolist_to_binary(Msg),
            EmailMsg = eMime:constructMail(User, "eTodo reminder",
                                           EmailAddr, EmailAddr, EmailAddr,
                                           Msg2),
            eSMTP:sendMail(EmailAddr, EmailAddr, EmailMsg)
    end.

initAlarm(AlarmCfg) ->
    FirstAlarm = {AlarmCfg#alarmCfg.startDate,
                  repairTime(AlarmCfg#alarmCfg.startTime)},
    AlarmCfg#alarmCfg{nextAlarm = FirstAlarm}.

setNext(AlarmCfg = #alarmCfg{nextAlarm = undefined}, _Now) ->
    initAlarm(AlarmCfg);
setNext(AlarmCfg = #alarmCfg{recurrence = none}, _Now) ->
    AlarmCfg#alarmCfg{nextAlarm = never};
setNext(AlarmCfg = #alarmCfg{endDate  = EndDate = {_,_,_}}, {Date, _Time})
  when (EndDate < Date) ->
    AlarmCfg#alarmCfg{nextAlarm = never};
setNext(AlarmCfg = #alarmCfg{nextAlarm = Next}, Now) when (Next > Now) ->
    AlarmCfg;
setNext(AlarmCfg = #alarmCfg{nextAlarm = Previous, recurrence = Rec}, Now) ->
    Next = addDateTime(Previous, recurence2DateTime(Rec) ),
    setNext(AlarmCfg#alarmCfg{nextAlarm = Next}, Now).


repairTime({H, M, S}) ->
    {H, M, S};
repairTime({H, M}) ->
    {H, M, 0};
repairTime(undefined) ->
    time().

recurence2DateTime(time15) ->    {{0, 0, 0}, {0, 15, 0}};
recurence2DateTime(time30) ->    {{0, 0, 0}, {0, 30, 0}};
recurence2DateTime(time1h) ->    {{0, 0, 0}, {1,  0, 0}};
recurence2DateTime(time2h) ->    {{0, 0, 0}, {2,  0, 0}};
recurence2DateTime(time4h) ->    {{0, 0, 0}, {4,  0, 0}};
recurence2DateTime(timeDay) ->   {{0, 0, 1}, {0,  0, 0}};
recurence2DateTime(timeWeek) ->  {{0, 0, 7}, {0,  0, 0}};
recurence2DateTime(timeMonth) -> {{0, 1, 0}, {0,  0, 0}};
recurence2DateTime(timeYear) ->  {{1, 0, 0}, {0,  0, 0}};
recurence2DateTime({Date, Time}) -> {Date, Time}.

