%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2012 by mikael <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eTodoDB).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,

         addReminder/1,
         addTodo/2,
         appendToPage/3,
         assignLists/3,
         clearUndo/0,
         delTodo/2,
         delTodo/3,
         delTodo/4,
         delReminder/1,
         getAllTodos/0,
         getAllUserInfos/0,
         getConnections/0,
         getConnection/1,
         getColumns/1,
         getETodos/5,
         getLists/2,
         getLoggedWork/2,
         getLoggedWork/3,
         getAllLoggedWork/1,
         getAllLoggedWorkInt/1,
         getAllLoggedWorkDate/1,
         getReminder/2,
         getReminders/1,
         getRow/2,
         getSubTodos/1,
         getTime/1,
         getTodo/1,
         getTodos/2,
         getTodosSharedWith/2,
         getUsers/0,
         getWorkDesc/1,
         getWorkDescAll/1,
         hasSubTodo/1,
         insertTodo/2,
         logWork/5,
         moveDown/4,
         moveUp/4,
         moveTodosToTaskList/3,
         newTodo/1,
         readListCfg/2,
         readListCfg/3,
         readUserCfg/1,
         redo/0,
         removeConnection/1,
         saveListCfg/3,
         saveListCfg/4,
         saveTime/3,
         saveUserCfg/1,
         saveWorkDesc/4,
         updateConnection/1,
         updateTodo/2,
         updateTodoNoDiff/2,
         updateTodoNoLocks/1,
         undo/0,
         undoStatus/0]).

%% Exported for unit testing.
-export([calcDiff/2, applyDiff/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([appendColumn/1,
         appendColumns/2]).

-define(SERVER, ?MODULE).

-define(Columns, [{uid,         0, ?uid},
                  {status,      1, ?status},
                  {priority,    2, ?prio},
                  {owner,       3, ?owner},
                  {dueTime,     4, ?dueTime},
                  {description, 5, ?description},
                  {comment,     6, ?comment},
                  {sharedWith,  7, ?sharedWith},
                  {createTime,  8, ?createTime},
                  {doneTime,    9, ?doneTimestamp}]).

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("inets/include/mod_auth.hrl").

-record(state, {undo = [], redo = [], opType = normal}).

-import(eTodoUtils, [makeStr/1, toStr/1, toStr/2, col/2, default/2]).

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

stop() ->
    gen_server:cast(?MODULE, stop).

updateConnection(ConCfg) ->
    gen_server:call(?MODULE, {updateConnection, ConCfg}).

removeConnection(ConCfg) ->
    gen_server:call(?MODULE, {removeConnection, ConCfg}).

addReminder(AlarmCfg) ->
    gen_server:call(?MODULE, {addReminder, AlarmCfg}).

addTodo(UserInfo, Todo) when Todo#todo.uid =/= undefined ->
    gen_server:call(?MODULE, {addTodo, toList(UserInfo), Todo}).

appendToPage(User, Type, Html) ->
    gen_server:cast(?MODULE, {appendToPage, User, Type, Html}).

assignLists(User, Uid, Lists) ->
    gen_server:call(?MODULE, {assignLists, User, Uid, Lists}).

delReminder(AlarmCfg) ->
    gen_server:call(?MODULE, {delReminder, AlarmCfg}).

delTodo(Uid, User) ->
    ePeerLock:getLock(Uid, User),
    gen_server:call(?MODULE, {delTodo, Uid}),
    ePeerLock:releaseLock(Uid, User).

delTodo(Uid, Parent, User) ->
    ePeerLock:getLock(Uid, User),
    gen_server:call(?MODULE, {delTodo, Uid, toList(Parent)}),
    ePeerLock:releaseLock(Uid, User).

delTodo(Uid, Parent, User, RemoveAll) ->
    ePeerLock:getLock(Uid, User),
    gen_server:call(?MODULE, {delTodo, Uid, toList(Parent), RemoveAll}),
    ePeerLock:releaseLock(Uid, User).

getAllTodos() ->
    gen_server:call(?MODULE, getAllTodos).

getAllUserInfos() ->
    gen_server:call(?MODULE, getAllUserInfos).

getConnections() ->
    gen_server:call(?MODULE, getConnections).

getConnection(User) ->
    gen_server:call(?MODULE, {getConnection, User}).

getColumns(User) ->
    gen_server:call(?MODULE, {getColumns, User}).

getETodos(User, ?defInbox, Filter, SearchText, Cfg) ->
    gen_server:call(?MODULE, {getETodos, User, ?defTaskList,
                              [?assigned | Filter], SearchText, Cfg});
getETodos(User, List, Filter, SearchText, Cfg) ->
    gen_server:call(?MODULE, {getETodos, User, List, Filter, SearchText, Cfg}).

getLists(User, Uid) ->
    gen_server:call(?MODULE, {getLists, User, Uid}).

getWorkDesc(Uid) ->
    gen_server:call(?MODULE, {getWorkDesc, Uid}).

getWorkDescAll(Uid) ->
    gen_server:call(?MODULE, {getWorkDescAll, Uid}).

getTime(Uid) ->
    gen_server:call(?MODULE, {getTime, Uid}).

getLoggedWork(User, Uid, Date) ->
    gen_server:call(?MODULE, {getLoggedWork, User, Uid, Date}).

getLoggedWork(User, Date) ->
    gen_server:call(?MODULE, {getLoggedWork, User, Date}).

getAllLoggedWork(Uid) ->
    gen_server:call(?MODULE, {getAllLoggedWork, Uid}).

getAllLoggedWorkInt(Uid) ->
    gen_server:call(?MODULE, {getAllLoggedWorkInt, Uid}).

getAllLoggedWorkDate(Uid) ->
    gen_server:call(?MODULE, {getAllLoggedWorkDate, Uid}).

getReminder(User, Uid) ->
    gen_server:call(?MODULE, {getReminder, User, Uid}).

getReminders(User) ->
    gen_server:call(?MODULE, {getReminders, User}).

getRow(User, List) ->
    gen_server:call(?MODULE, {getRow, User, toList(List)}).

getTodo(Uid) ->
    gen_server:call(?MODULE, {getTodo, Uid}).

getTodos(User, List) ->
    gen_server:call(?MODULE, {getTodos, User, toList(List)}).

getTodosSharedWith(User1, User2) ->
    gen_server:call(?MODULE, {getTodosSharedWith, User1, User2}).

getUsers() ->
    gen_server:call(?MODULE, getUsers).

getSubTodos(Uid) ->
    gen_server:call(?MODULE, {getSubTodos, Uid}).

hasSubTodo(Uid) ->
    gen_server:call(?MODULE, {hasSubTodo, Uid}).

insertTodo(UserInfo, Todo) ->
    gen_server:call(?MODULE, {insertTodo, toList(UserInfo), Todo}).

logWork(User, Uid, Date, Hours, Minutes) ->
    gen_server:call(?MODULE, {logWork, User, Uid, Date, Hours, Minutes}).

moveUp(User, Parent, Uid1, Uid2) ->
    gen_server:call(?MODULE, {moveUp, User, toList(Parent), Uid1, Uid2}).

moveDown(User, Parent, Uid1, Uid2) ->
    gen_server:call(?MODULE, {moveDown, User, toList(Parent), Uid1, Uid2}).

newTodo(#todo{uid = Uid}) ->
    (getTodo(Uid) == undefined);
newTodo(#diff{uid = Uid}) ->
    (getTodo(Uid) == undefined).

readListCfg(UserName, ColumnName, Key) ->
    gen_server:call(?MODULE, {readListCfg, UserName, ColumnName, Key}).

readListCfg(UserName, Key) ->
    gen_server:call(?MODULE, {readListCfg, UserName, Key}).

readUserCfg(UserName) ->
    gen_server:call(?MODULE, {readUserCfg, UserName}).

moveTodosToTaskList(User, ToList, FromLists) ->
    gen_server:call(?MODULE, {moveTodosToTaskList, User, ToList, FromLists}).

saveListCfg(UserName, ColumnName, Key, Value) ->
    gen_server:call(?MODULE, {saveListCfg, UserName, ColumnName, Key, Value}).

saveListCfg(UserName, Key, Value) ->
    gen_server:call(?MODULE, {saveListCfg, UserName, Key, Value}).

saveUserCfg(UserCfg) ->
    gen_server:call(?MODULE, {saveUserCfg, UserCfg}).

saveWorkDesc(Uid, Desc, ShowInWL, ShowInTL) ->
    gen_server:call(?MODULE, {saveWorkDesc, Uid, Desc, ShowInWL, ShowInTL}).

saveTime(Uid, Estimate, Remaining) ->
    gen_server:call(?MODULE, {saveTime, Uid, Estimate, Remaining}).

updateTodoNoDiff(User, Todo = #todo{sharedWith = Users}) ->
    Users2 = default(Users, []),
    ePeerLock:getLocks(Todo#todo.uid, User, Users2),
    gen_server:call(?MODULE, {updateTodo, Todo}),
    ePeerEM:todoUpdated(User, Users2, Todo),
    ePeerLock:releaseLocks(Todo#todo.uid, User, Users2).

updateTodo(User, Todo = #todo{sharedWith = Users})
  when Todo#todo.uid =/= undefined ->
    Users2 = default(Users, []),
    ePeerLock:getLocks(Todo#todo.uid, User, Users2),
    Diff = gen_server:call(?MODULE, {updateTodo, Todo}),
    case Diff of
        #diff{diff = []} ->
            %% No difference, do not send todoUpdated signal.
            ok;
        _ ->
            %% Calculated diff, send to other peer.
            ePeerEM:todoUpdated(User, Users2, Diff)
    end,
    ePeerLock:releaseLocks(Todo#todo.uid, User, Users2).

updateTodoNoLocks(Todo) when Todo#todo.uid =/= undefined ->
    gen_server:call(?MODULE, {updateTodo, Todo});
updateTodoNoLocks(Todo) when Todo#diff.uid =/= undefined ->
    gen_server:call(?MODULE, {updateTodo, Todo}).

clearUndo() ->
    gen_server:call(?MODULE, clearUndo).

undo() ->
    gen_server:call(?MODULE, undo).

undoStatus() ->
    gen_server:call(?MODULE, undoStatus).

redo() ->
    gen_server:call(?MODULE, redo).

appendColumn(Table) ->
    appendColumns(Table, [undefined]).

appendColumns(_Table, []) ->
    ok;
appendColumns(Table, Columns) ->
    eLog:log(debug, ?MODULE, appendColumns, [Table, Columns],
             "Appending columns.", ?LINE),
    Fun = fun(X) ->
                  list_to_tuple(tuple_to_list(X) ++ Columns)
          end,
    mnesia:transform_table(Table, Fun, recordInfo(Table)).

deleteColumns(Table, Len) ->
    eLog:log(debug, ?MODULE, deleteColumns, [Table, Len],
             "Deleting columns.", ?LINE),
    Fun = fun(X) ->
                  list_to_tuple(lists:sublist(tuple_to_list(X), 1, Len))
          end,
    mnesia:transform_table(Table, Fun, recordInfo(Table)).

recordInfo(todo)        -> record_info(fields, todo);
recordInfo(alarmCfg)    -> record_info(fields, alarmCfg);
recordInfo(userInfo)    -> record_info(fields, userInfo);
recordInfo(userCfg)     -> record_info(fields, userCfg);
recordInfo(listCfg)     -> record_info(fields, listCfg);
recordInfo(conCfg)      -> record_info(fields, conCfg);
recordInfo(logWork)     -> record_info(fields, logWork);
recordInfo(logTime)     -> record_info(fields, logTime);
recordInfo(workDesc)    -> record_info(fields, workDesc);
recordInfo(messages)    -> record_info(fields, messages);
recordInfo(httpd_user)  -> record_info(fields, httpd_user);
recordInfo(httpd_group) -> record_info(fields, httpd_group).

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
    ExistingTables = mnesia:system_info(tables),
    TablesToCreate = [todo, userInfo, conCfg, userCfg, logWork, logTime,
                      workDesc, listCfg, alarmCfg, httpd_user, httpd_group,
                      messages],

    PDlg = createProgressDialog(),
    createTables(ExistingTables, [schema]),
    waitForTables(TablesToCreate, PDlg, 0),
    createTables(ExistingTables, TablesToCreate),
    mnesia:wait_for_tables(TablesToCreate, 30000),
    checkAndRepair(),
    updateProgressDialog(PDlg, 30),
    {ok, #state{}}.

createProgressDialog() ->
    case application:get_env(eTodo, mode) of
        {ok, "noGui"} ->
            undefined;
        _ ->
            wx:new(),
            wxProgressDialog:new("eTodo", "Creating tables...", [{maximum, 30}])
    end.

updateProgressDialog(undefined, _Progress) ->
    ok;
updateProgressDialog(PDlg, Progress) ->
    wxProgressDialog:update(PDlg, Progress).

waitForTables(_TablesToCreate, _PDlg, 30) ->
    ok;
waitForTables(TablesToCreate, PDlg, Prog) ->
    updateProgressDialog(PDlg, Prog),
    case mnesia:wait_for_tables(TablesToCreate, 1000) of
        ok ->
            ok;
        _ ->
            waitForTables(TablesToCreate, PDlg, Prog + 1)
    end.


createTables(_Existing, []) ->
    ok;
createTables(Existing, [Table|Rest]) ->
    Result = createTable(lists:member(Table, Existing), Table),
    eLog:log(debug, ?MODULE, createTables, [Table, Result],
             "Creating table.", ?LINE),
    createTables(Existing, Rest).

createTable(_, schema) ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start();

createTable(false, alarmCfg) ->
    mnesia:create_table(alarmCfg, [{attributes, recordInfo(alarmCfg)},
                                   {disc_copies, [node()]},
                                   {index, [userName]}, {type, bag}]);
createTable(false, userInfo) ->
    mnesia:create_table(userInfo, [{attributes, recordInfo(userInfo)},
                                   {disc_copies, [node()]},
                                   {index, [uid, parent]}, {type, bag}]);
createTable(false, logWork) ->
    mnesia:create_table(logWork, [{attributes, recordInfo(logWork)},
                                  {disc_copies, [node()]},
                                  {index, [uid, date]},
                                  {type, bag}]);
createTable(false, httpd_user) ->
    mnesia:create_table(httpd_user, [{type, bag}, {disc_copies, [node()]},
                                     {attributes, recordInfo(httpd_user)}]);
createTable(false, httpd_group) ->
    mnesia:create_table(httpd_group, [{type, bag}, {disc_copies, [node()]},
                                      {attributes, recordInfo(httpd_group)}]);
createTable(false, Table) ->
    mnesia:create_table(Table, [{attributes, recordInfo(Table)},
                                {disc_copies, [node()]}]);
createTable(true, Table) ->
    OldAtt  = mnesia:table_info(Table, attributes),
    NewAtt  = recordInfo(Table),
    DiffAtt = NewAtt -- OldAtt,
    case OldAtt of
        NewAtt ->
            %% No change
            ok;
        _ ->
            case DiffAtt of
                [] ->
                    %% Columns deleted
                    deleteColumns(Table, length(NewAtt) + 1);
                _ ->
                    %% Columns added
                    Columns = columns(DiffAtt),
                    appendColumns(Table, Columns)
            end
    end.

columns(Columns) ->
    columns(Columns, []).

columns([], Values) ->
    lists:reverse(Values);
columns([Column|Rest], Values) ->
    columns(Rest, [value(Column)|Values]).

value(showLogin)       -> false;
value(filter)          -> [];
value(splitterMain)    -> 85;
value(splitterComment) -> 250;
value(splitterMsg)     -> 100;
value(_Column)         -> undefined.

getLists(User) ->
    Result = match(#userInfo{userName = User, _ = '_'}),
    getLists2(Result, []).

getLists2([], Lists) ->
    Lists;
getLists2([#userInfo{parent = Parent}|Rest], Lists) ->
    getLists2(Rest, lists:umerge([Parent], Lists)).

checkAndRepair() ->
    Res   = match(#conCfg{_ = '_'}),
    Users = [User || #conCfg{userName = User} <- Res],
    checkAndRepair(Users).

checkAndRepair([]) ->
    ok;
checkAndRepair([User|Rest]) ->
    Lists = getLists(User),
    checkAndRepair(User, Lists),
    checkAndRepair(Rest).

checkAndRepair(_User, []) ->
    ok;
checkAndRepair(User, [List|Lists]) ->
    Result  = match(#userInfo{userName = User,
                              parent   = List,
                              _        = '_'}),
    Result2 = lists:keysort(#userInfo.row, Result),
    checkAndRepair(User, Result2, 0),
    checkAndRepair(User, Lists).

checkAndRepair(_User, [], _Row) ->
    ok;
checkAndRepair(User, [#userInfo{row = Row}|Rest], Row) ->
    checkAndRepair(User, Rest, Row + 1);
checkAndRepair(User, [UserInfo|Rest], Row) ->
    case matchOne(UserInfo#userInfo{row = '_'}) of
        #userInfo{} ->
            %% Table has user info but with wrong row.
            UserInfo2 = UserInfo#userInfo{row = Row},
            eLog:log(debug, ?MODULE, checkAndRepair, [UserInfo, UserInfo2],
                     "Repairing table.", ?LINE),
            mnesia:transaction(
              fun() ->
                      delete_object(UserInfo),
                      mnesia:write(UserInfo2)
              end);
        undefined ->
            %% Table has duplicates of same uid in same list.
            mnesia:transaction(
              fun() ->
                      delete_object(UserInfo)
              end)
    end,
    checkAndRepair(User, Rest, Row + 1).


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
handle_call({updateConnection, #conCfg{userName = ""}}, _From, State) ->
    {reply, ok, State};
handle_call({updateConnection, #conCfg{userName = undefined}}, _From, State) ->
    {reply, ok, State};
handle_call({updateConnection, ConCfg}, _From, State) ->
    Result = mnesia:transaction(fun() -> mnesia:write(ConCfg) end),
    {reply, Result, State};
handle_call({removeConnection, #conCfg{userName = UserName}}, _From, State) ->
    Result = mnesia:transaction(fun() -> mnesia:delete({conCfg, UserName}) end),
    {reply, Result, State};
handle_call(Event = {addReminder, Reminder}, _From, State) ->
    {Result, Object} = doAddReminder(Reminder),
    State2 = addToUndo({Event, {delReminder, Object}}, State),
    {reply, Result, State2};
handle_call(Event = {addTodo, UserInfo, Todo}, _From, State) ->
    Result = writeTodo(UserInfo, Todo),
    State2 = addToUndo({Event, {delTodo, UserInfo#userInfo.uid}}, State),
    {reply, Result, State2};
handle_call(Event = {assignLists, User, Uid, Lists}, _From, State) ->
    {Result, UndoInfo} = doAssignLists(User, Uid, Lists),
    State2 = addToUndo({Event, {undoAssignLists, User, Uid, UndoInfo}}, State),
    {reply, Result, State2};
handle_call({delReminder, Reminder}, _From, State) ->
    Result = mnesia:transaction(fun() -> delete_object(Reminder) end),
    {reply, Result, State};
handle_call(Event = {delTodo, Uid}, _From, State) ->
    {Result, UndoInfo} = deleteTodo(Uid),
    State2 = addToUndo({Event, {undoDelTodo, UndoInfo}}, State),
    {reply, Result, State2};
handle_call(Event = {delTodo, Uid, Parent}, _From, State) ->
    {Result, UndoInfo} = deleteTodo(Uid, Parent, true),
    State2 = addToUndo({Event, {undoDelTodo, UndoInfo}}, State),
    {reply, Result, State2};
handle_call(Event = {delTodo, Uid, Parent, RemoveAll}, _From, State) ->
    {Result, UndoInfo} = deleteTodo(Uid, Parent, RemoveAll),
    State2 = addToUndo({Event, {undoDelTodo, UndoInfo}}, State),
    {reply, Result, State2};
handle_call(getAllTodos, _From, State) ->
    Result = match(#todo{_ = '_'}),
    {reply, Result, State};
handle_call(getAllUserInfos, _From, State) ->
    Result = match(#userInfo{_ = '_'}),
    {reply, Result, State};
handle_call(getConnections, _From, State) ->
    Result  = match(#conCfg{_ = '_'}),
    SortFun =
        fun (ConCfg) ->
                Dist = ConCfg#conCfg.distance,
                ((Dist ==  undefined) or (Dist == 1))  and
                                                         (ConCfg#conCfg.host     =/= undefined) and
                                                                                                  (ConCfg#conCfg.port     =/= undefined)
        end,
    Result2 = lists:filter(SortFun, Result),
    {reply, Result2, State};
handle_call({getConnection, User}, _From, State) ->
    Result = read(conCfg, User),
    {reply, Result, State};
handle_call({getColumns, User}, _From, State) ->
    Result  = getColumns(User, ?Columns),
    {reply, Result, State};
handle_call({getETodos, User, Parent, Filter, SearchText, Cfg}, _From, State) ->
    Cols    = getColumns(User, ?Columns),
    Result  = match(#userInfo{userName = User,
                              parent   = Parent,
                              _        = '_'}),
    Result2 = lists:keysort(#userInfo.row, Result),
    Result3 =
        case lists:member(?assigned, Filter) of
            true ->
                Fun = fun(UserInfo) ->
                              case matchOne(UserInfo#userInfo{parent = '_',
                                                              row    = '_'}) of
                                  undefined ->
                                      false;
                                  _ ->
                                      true
                              end
                      end,
                lists:filter(Fun, Result2);
            false ->
                Result2
        end,
    Result4 = [makeETodo(Uid, User, Cols) || #userInfo{uid = Uid} <- Result3],
    Result5 = lists:filter(fun(ETodo) ->
                                   filter(ETodo, Filter)
                                       andalso search(ETodo, SearchText, Cfg)
                           end, Result4),
    Result6 = advSearch(User, SearchText, Result5),
    Result7 = sort(User, Result6),
    {reply, Result7, State};
handle_call({getLists, User, Uid}, _From, State) ->
    Lists = doGetLists(User, Uid),
    {reply, Lists, State};
handle_call({getLoggedWork, User, Uid, Date}, _From, State) ->
    LoggedWork = matchOne(#logWork{userName = User, uid = Uid,
                                   date     = Date,   _ = '_'}),

    Result = case LoggedWork of
                 undefined ->
                     {Date, 0, 0};
                 _ ->
                     {LoggedWork#logWork.date,
                      default(LoggedWork#logWork.hours, 0),
                      default(LoggedWork#logWork.minutes, 0)}
             end,
    {reply, Result, State};
handle_call({getAllLoggedWork, Uid}, _From, State) ->
    LoggedWork = match(#logWork{uid = Uid, _ = '_'}),
    Result     = tot(LoggedWork),
    {reply, Result, State};
handle_call({getAllLoggedWorkInt, Uid}, _From, State) ->
    LoggedWork = match(#logWork{uid = Uid, _ = '_'}),
    Result     = totInt(LoggedWork),
    {reply, Result, State};
handle_call({getAllLoggedWorkDate, Uid}, _From, State) ->
    LoggedWork = match(#logWork{uid = Uid, _ = '_'}),
    DateSorted = lists:keysort(#logWork.date, LoggedWork),
    Result = [toStr({Work#logWork.date,
                     {Work#logWork.hours,
                      Work#logWork.minutes}}) || Work <- DateSorted,
                                                 (Work#logWork.hours + Work#logWork.minutes) =/= 0],
    {reply, Result, State};
handle_call({getWorkDesc, Uid}, _From, State) ->
    Result = default(matchOne(#workDesc{uid = Uid, _ = '_'}), #workDesc{}),
    {reply, Result#workDesc.shortDesc, State};
handle_call({getWorkDescAll, Uid}, _From, State) ->
    Result = default(matchOne(#workDesc{uid = Uid, _ = '_'}), #workDesc{}),
    #workDesc{showInWorkLog = ShowInWorkLog,
              showInTimeLog = ShowInTimeLog,
              shortDesc     = ShortDescription} = Result,
    {reply, {ok, ShortDescription, ShowInWorkLog, ShowInTimeLog}, State};
handle_call({getTime, Uid}, _From, State) ->
    Result = default(matchOne(#logTime{uid = Uid, _ = '_'}), #logTime{}),
    {reply, {default(Result#logTime.timeEstimate, 0),
             default(Result#logTime.timeRemaining, 0)}, State};
handle_call({saveWorkDesc, Uid, WorkDesc, ShowInWorkLog, ShowInTimeLog},
            _From, State) ->
    TS = fun() ->
                 mnesia:write(#workDesc{uid           = Uid,
                                        shortDesc     = WorkDesc,
                                        showInTimeLog = ShowInTimeLog,
                                        showInWorkLog = ShowInWorkLog})
         end,
    Result = mnesia:transaction(TS),
    {reply, Result, State};
handle_call({saveTime, Uid, Estimate, Remaining}, _From, State) ->
    TS = fun() ->
                 mnesia:write(#logTime{uid           = Uid,
                                       timeEstimate  = Estimate,
                                       timeRemaining = Remaining})
         end,
    Result = mnesia:transaction(TS),
    {reply, Result, State};
handle_call({getLoggedWork, User, Date}, _From, State) ->
    LoggedWork = match(#logWork{userName = User, date = Date, _ = '_'}),
    ShowInWL   = match(#workDesc{showInWorkLog = true, _ = '_'}),
    AddToWL    = addToLoggedWork(LoggedWork, ShowInWL),
    Result = [{LW#logWork.uid, LW#logWork.hours, LW#logWork.minutes} ||
                 LW <- AddToWL, showInWorkLog(LW)],
    {reply, Result, State};
handle_call({logWork, User, Uid, Date, Hours, Minutes}, _From, State) ->
    LoggedWork = #logWork{userName = User,
                          uid      = Uid,
                          date     = Date,
                          hours    = Hours,
                          minutes  = Minutes},
    Result = logWorkToDB(LoggedWork),
    {reply, Result, State};
handle_call({getReminder, User, Uid}, _From, State) ->
    Reminder = matchOne(#alarmCfg{userName = User, uid = Uid, _ = '_'}),
    {reply, Reminder, State};
handle_call({getReminders, User}, _From, State) ->
    Reminders = match(#alarmCfg{userName = User, _ = '_'}),
    {reply, Reminders, State};
handle_call({getRow, User, List}, _From, State) ->
    Result = doGetRow(User, List),
    {reply, Result, State};
handle_call({getTodo, Uid}, _From, State) ->
    Result = read(todo, Uid),
    {reply, Result, State};
handle_call({getTodos, User, Parent}, _From, State) ->
    Result  = match(#userInfo{userName = User,
                              parent   = Parent,
                              _        = '_'}),
    Result2 = lists:keysort(#userInfo.row, Result),
    Result3 = [read(todo, Uid) || #userInfo{uid = Uid} <- Result2],
    {reply, Result3, State};
handle_call({getTodosSharedWith, User1, User2}, _From, State) ->
    Result1 = match(#todo{_ = '_'}),
    Result2 = getTodosSharedWith(User1, User2, Result1),
    {reply, Result2, State};
handle_call(getUsers, _From, State) ->
    Result1 = match(#conCfg{_ = '_'}),
    Result2 = [User || #conCfg{userName = User} <- Result1],
    {reply, Result2, State};
handle_call({getSubTodos, Uid}, _From, State) ->
    Result1 = match(#userInfo{parent = Uid, _ = '_'}),
    Result2 = [UserInfo#userInfo.uid || UserInfo <- Result1],
    {reply, Result2, State};
handle_call({hasSubTodo, Uid}, _From, State) ->
    Result1 = match(#userInfo{parent = Uid, _ = '_'}),
    Result2 = (Result1 =/= []),
    {reply, Result2, State};
handle_call(Event = {insertTodo, UserInfo, Todo}, _From, State) ->
    insertRow(UserInfo),
    Todo2  = Todo#todo{createTime = {date(), time()}},
    Result = writeTodo(UserInfo, Todo2),
    State2  = addToUndo({Event, {delTodo, UserInfo#userInfo.uid}}, State),
    {reply, Result, State2};
handle_call(Event = {moveDown, User, Parent, Uid1, Uid2}, _From, State) ->
    Result  = swapRows(User, Parent, Uid1, Uid2),
    State2  = addToUndo({Event, {moveUp, User, Parent, Uid2, Uid1}}, State),
    {reply, Result, State2};
handle_call(Event = {moveUp, User, Parent, Uid1, Uid2}, _From, State) ->
    Result = swapRows(User, Parent, Uid1, Uid2),
    State2  = addToUndo({Event, {moveDown, User, Parent, Uid2, Uid1}}, State),
    {reply, Result, State2};
handle_call({readListCfg, UserName, ColumnName, Field}, _From, State) ->
    Config = readConfig(listCfg, UserName, ColumnName),
    Result = getField(listCfg, Field, Config),
    {reply, Result, State};
handle_call({readListCfg, UserName, Field}, _From, State) ->
    Config = readConfig(listCfg, UserName, Field),
    Result = getField(listCfg, Field, Config),
    {reply, Result, State};
handle_call({readUserCfg, UserName}, _From, State) ->
    Config = default(read(userCfg, UserName), #userCfg{userName = UserName}),
    {reply, Config, State};
handle_call({moveTodosToTaskList, User, List, Lists}, _From, State) ->
    Result = doMoveTodosToTaskList(User, List, Lists),
    {reply, Result, State};
handle_call(redo, _From, State = #state{redo = []}) ->
    {reply, empty, State};
handle_call(redo, _From, State = #state{redo = [RedoInfo|Rest],
                                        undo = UndoList}) ->
    {Event, _UndoInfo} = RedoInfo,
    eLog:log(debug, ?MODULE, redo, [Event, Rest, UndoList],
             "Undo executed.", ?LINE),
    handle_call(Event, redo,
                State#state{undo   = [RedoInfo|UndoList],
                            redo   = Rest,
                            opType = redo});
handle_call(Event = {saveListCfg, UserName, Field, Value}, _From, State) ->
    Config1 = readConfig(listCfg, UserName, Field),
    Config2 = Config1#listCfg{key = {UserName, Field}, userName = UserName},
    Config3 = setField(listCfg, Field, Value, Config2),
    Result  = mnesia:transaction(fun() -> mnesia:write(Config3) end),
    State2  = addToUndo({Event, {undoSaveCfg, Config1}}, State),
    {reply, Result, State2};
handle_call(Event = {saveListCfg, UserName, Column, Field, Value},
            _From, State) ->
    Config1 = readConfig(listCfg, UserName, Column),
    Config2 = Config1#listCfg{key = {UserName, Column},
                              userName = UserName, columnName = Column},
    Config3 = setField(listCfg, Field, Value, Config2),
    Result  = mnesia:transaction(fun() -> mnesia:write(Config3) end),
    State2  = addToUndo({Event, {undoSaveCfg, Config1}}, State),
    {reply, Result, State2};
handle_call({saveUserCfg, UserCfg}, _From, State) ->
    Result = mnesia:transaction(fun() -> mnesia:write(UserCfg) end),
    {reply, Result, State};
handle_call(Event = {updateTodo, UserInfo = #userInfo{}}, _From, State) ->
    Object = matchOne(UserInfo#userInfo{row = '_'}),
    Result = mnesia:transaction(
               fun() ->
                       delete_object(Object),
                       mnesia:write(UserInfo)
               end),
    State2  = addToUndo({Event, {updateTodo, Object}}, State),
    {reply, Result, State2};
handle_call(Event = {updateTodo, Todo = #todo{}}, _From, State) ->
    Object = read(todo, Todo#todo.uid),
    Result = calcDiff(Todo, Object),
    case Result of
        #diff{diff = []} ->
            {reply, Result, State};
        Result ->
            Diff = calcDiff(Object, Todo),
            mnesia:transaction(fun() ->
                                       mnesia:write(Todo)
                               end),
            State2  = addToUndo({Event, {updateTodo, Diff}}, State),
            {reply, Result, State2}
    end;
handle_call(Event = {updateTodo, Todo = #diff{}}, _From, State) ->
    Object = read(todo, Todo#diff.uid),
    Todo2  = applyDiff(Todo, Object),

    Result = mnesia:transaction(fun() ->
                                        mnesia:write(Todo2)
                                end),
    State2  = addToUndo({Event, {updateTodo, calcDiff(Object, Todo2)}}, State),
    {reply, Result, State2};
handle_call(clearUndo, _From, State) ->
    {reply, ok, State#state{undo = [], redo = []}};
handle_call(undo, _From, State = #state{undo = []}) ->
    {reply, empty, State};
handle_call(undo, From, State = #state{redo = RedoList,
                                       undo = [UndoInfo|Rest]}) ->
    {_RedoInfo, Event} = UndoInfo,
    eLog:log(debug, ?MODULE, undo, [Event, Rest, RedoList],
             "Undo executed.", ?LINE),
    handle_call(Event, From,
                State#state{undo   = Rest,
                            redo   = [UndoInfo|RedoList],
                            opType = undo});
handle_call(undoStatus, _From, State = #state{undo = L1, redo = L2}) ->
    {reply, {L1 =/= [], L2 =/= []}, State};
handle_call({undoDelTodo, UndoInfo}, _From, State) ->
    Reply = undoDelTodo(UndoInfo),
    {reply, Reply, State};
handle_call({undoAssignLists, User, Uid, UndoInfo}, _From, State) ->
    Reply = undoAssignLists(User, Uid, UndoInfo),
    {reply, Reply, State};
handle_call({undoSaveCfg, Config}, _From, State) ->
    Result  = mnesia:transaction(fun() -> mnesia:write(Config) end),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({appendToPage, User, Type, Html}, State) ->
    TS = fun() ->
                 mnesia:write(#messages{key       = make_ref(),
                                        timestamp = erlang:timestamp(),
                                        userName  = User,
                                        type      = Type,
                                        message   = zlib:zip(Html)})
         end,
    mnesia:transaction(TS),
    {noreply, State};
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

%%====================================================================
%% ?defInbox is ?defTaskList with a filter.
%%====================================================================
toList(UserInfo = #userInfo{parent = ?defInbox}) ->
    UserInfo#userInfo{parent = ?defTaskList};
toList(?defInbox) -> ?defTaskList;
toList(Other)     -> Other.

%%====================================================================
%% Add reminder to database.
%%====================================================================
doAddReminder(Reminder) ->
    Uid     = Reminder#alarmCfg.uid,
    User    = Reminder#alarmCfg.userName,
    Pattern = #alarmCfg{uid = Uid, userName = User, _ = '_'},
    Object  = matchOne(Pattern),
    Result  =
        mnesia:transaction(
          fun() ->
                  delete_object(Object),
                  mnesia:write(Reminder)
          end),
    {Result, Object}.

%%====================================================================
%% Search database for pattern, if we match none or more than on
%% return undefined, otherwise return row that matched.
%%====================================================================
matchOne(Pattern) ->
    case match(Pattern) of
        [Value] ->
            Value;
        _ ->
            undefined
    end.

%%====================================================================
%% Search database for pattern, return all the rows that matched.
%%====================================================================
match(Pattern) ->
    case mnesia:transaction(fun() -> mnesia:match_object(Pattern) end) of
        {atomic, Result} ->
            Result;
        _ ->
            []
    end.

%%====================================================================
%% Read a value of a field in a table, if we find it return it
%% otherwise return undefined.
%%====================================================================
read(Table, Key) ->
    Result = mnesia:transaction(fun() -> mnesia:read(Table, Key, read) end),
    case Result of
        {atomic, [Value]} ->
            Value;
        _ ->
            undefined
    end.

%%====================================================================
%% Log work to database
%%====================================================================
logWorkToDB(LogWork = #logWork{userName = User, uid = Uid, date = Date}) ->
    case matchOne(#logWork{userName = User, uid = Uid, date = Date, _  = '_'}) of
        undefined ->
            mnesia:transaction(
              fun() ->
                      mnesia:write(LogWork)
              end);
        OldLoggedWork ->
            mnesia:transaction(
              fun() ->
                      mnesia:delete_object(OldLoggedWork),
                      mnesia:write(LogWork)
              end)
    end.

%%====================================================================
%% Write task to database, write both user info and task info in the
%% same transaction.
%%====================================================================
writeTodo(UserInfo = #userInfo{parent = ?defTaskList}, Todo) ->
    case matchOne(UserInfo#userInfo{parent = ?defTaskList, row = '_'}) of
        undefined ->
            mnesia:transaction(
              fun() ->
                      mnesia:write(UserInfo),
                      mnesia:write(Todo)
              end);
        OldTodo ->
            mnesia:transaction(fun() ->
                                       delete_object(OldTodo),
                                       mnesia:write(UserInfo),
                                       mnesia:write(Todo)
                               end)
    end;
writeTodo(UserInfo = #userInfo{userName = User}, Todo) ->
    case matchOne(UserInfo#userInfo{parent = ?defTaskList, row = '_'}) of
        undefined ->
            Row       = doGetRow(User, ?defTaskList),
            UserInfo2 = UserInfo#userInfo{parent = ?defTaskList, row = Row},
            mnesia:transaction(
              fun() ->
                      mnesia:write(UserInfo),
                      mnesia:write(UserInfo2),
                      mnesia:write(Todo)
              end);
        _ ->
            mnesia:transaction(fun() ->
                                       mnesia:write(UserInfo),
                                       mnesia:write(Todo)
                               end)
    end.

%%====================================================================
%% Delete all database info regarding a task.
%%====================================================================
deleteTodo(Uid, ?defTaskList, true) ->
    deleteTodo(Uid);
deleteTodo(Uid, Parent, _) ->
    Objects1 = match(#userInfo{uid = Uid, _ = '_'}),
    Objects2 = match(#userInfo{uid = Uid, parent = Parent, _ = '_'}),
    case (Objects1 == Objects2) of
        true ->
            U1 = read(todo, Uid),
            {mnesia:transaction(
               fun() ->
                       delete_object(U1),
                       [deleteObject(Object) || Object <- Objects1]
               end),
             {U1, Objects1}};
        false ->
            {mnesia:transaction(
               fun() ->
                       [deleteObject(Object) || Object <- Objects2]
               end),
             Objects2}
    end.

deleteTodo(Uid) ->
    U1      = read(todo, Uid),
    Objects = match(#userInfo{uid = Uid, _ = '_'}),
    {mnesia:transaction(
       fun() ->
               delete_object(U1),
               [deleteObject(Object) || Object <- Objects]
       end),
     {U1, Objects}}.

undoDelTodo({U1, Objects}) ->
    mnesia:transaction(
      fun() ->
              mnesia:write(U1),
              [insertObject(Object) || Object <- Objects]
      end).

deleteObject(Object = #userInfo{row      = Row,
                                parent   = Parent,
                                userName = User}) ->
    UserInfos = match(#userInfo{userName = User, parent = Parent, _ = '_'}),
    updateRow(UserInfos, {removed, Row}),
    delete_object(Object).

insertObject(Object = #userInfo{row      = Row,
                                parent   = Parent,
                                userName = User}) ->
    UserInfos = match(#userInfo{userName = User, parent = Parent, _ = '_'}),
    updateRow(UserInfos, {added, Row}),
    mnesia:write(Object).

updateRow([], _) ->
    ok;
updateRow([UserInfo|Rest],{added, Index}) ->
    Row = UserInfo#userInfo.row,
    case Index =< Row of
        true ->
            mnesia:transaction(
              fun() ->
                      delete_object(UserInfo),
                      mnesia:write(UserInfo#userInfo{row = Row + 1})
              end),
            updateRow(Rest, {added, Index});
        false ->
            updateRow(Rest, {added, Index})
    end;
updateRow([UserInfo|Rest],{removed, Index}) ->
    Row = UserInfo#userInfo.row,
    case Index < Row of
        true ->
            mnesia:transaction(
              fun() ->
                      delete_object(UserInfo),
                      mnesia:write(UserInfo#userInfo{row = Row - 1})
              end),
            updateRow(Rest, {removed, Index});
        false ->
            updateRow(Rest, {removed, Index})
    end.

delete_object(undefined) -> ok;
delete_object(Object)    ->
    mnesia:delete_object(Object).

%%====================================================================
%% Insert row at the begining of the list, update row on all following.
%%====================================================================
insertRow(#userInfo{userName = User, parent = Parent, row = Row}) ->
    UserInfos = match(#userInfo{userName = User,
                                parent   = Parent, _ = '_'}),
    updateRow(UserInfos, {added, Row}).

readConfig(Table, UserName, Key) ->
    case {Table, read(Table, {UserName, Key})} of
        {userCfg, undefined} ->
            #userCfg{userName = UserName};
        {listCfg, undefined} ->
            #listCfg{userName = UserName};
        {Table, Config} ->
            Config
    end.

%%====================================================================
%% Get/Set a record field.
%%====================================================================
getField(Field, ETodo) ->
    Fields = record_info(fields, etodo),
    getField(Fields, Field, ETodo, 2).

getField(listCfg, Field, Config) ->
    Fields = record_info(fields, listCfg),
    getField(Fields, Field, Config, 2);
getField(userCfg, Field, Config) ->
    Fields = record_info(fields, userCfg),
    getField(Fields, Field, Config, 2).

getField([], _Field, _Config, _Index)     -> undefined;
getField([Field|_], Field, Config, Index) -> element(Index, Config);
getField([_Field|Rest], Field, Config, Index) ->
    getField(Rest, Field, Config, Index + 1).


setField(userCfg, Field, Value, Config) ->
    Fields = record_info(fields, userCfg),
    setField(Fields, Field, Value, Config, 2);
setField(listCfg, Field, Value, Config) ->
    Fields = record_info(fields, listCfg),
    setField(Fields, Field, Value, Config, 2).

setField([], _Field, _Value, _Config, _Index) ->
    undefined;
setField([Field|_], Field, Value, Config, Index) ->
    setelement(Index, Config, Value);
setField([_Field|Rest], Field, Value, Config, Index) ->
    setField(Rest, Field, Value, Config, Index + 1).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
getCol(User, Column, Def) ->
    case getField(listCfg, order, readConfig(listCfg, User, Column)) of
        Num when is_integer(Num) ->
            Num;
        _Else ->
            Def
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
addToUndo(_UndoInfo, State = #state{opType = redo}) ->
    State#state{opType = normal};
addToUndo(_UndoInfo, State = #state{opType = undo}) ->
    State#state{opType = normal};
addToUndo({{updateTodo, _}, {updateTodo, #todo{priority   = undefined,
                                               status     = undefined,
                                               doneTime   = undefined,
                                               dueTime    = undefined,
                                               comment    = undefined,
                                               progress   = undefined,
                                               sharedWith = undefined,
                                               owner      = undefined}}},
          State) ->
    State;
addToUndo(UndoInfo, State = #state{undo = UndoList}) ->
    State#state{undo = [UndoInfo|UndoList]}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
swapRows(User, Parent, Uid1, Uid2) ->
    UserInfo1 = matchOne(#userInfo{userName = User,
                                   uid      = Uid1,
                                   parent   = Parent, _ = '_'}),
    UserInfo2 = matchOne(#userInfo{userName = User,
                                   uid      = Uid2,
                                   parent   = Parent, _ = '_'}),

    UserInfoRow1 = UserInfo1#userInfo.row,
    UserInfoRow2 = UserInfo2#userInfo.row,

    NewUserInfo1 = UserInfo1#userInfo{row = UserInfoRow2},
    NewUserInfo2 = UserInfo2#userInfo{row = UserInfoRow1},

    mnesia:transaction(fun() ->
                               delete_object(UserInfo1),
                               delete_object(UserInfo2),
                               mnesia:write(NewUserInfo1),
                               mnesia:write(NewUserInfo2)
                       end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
makeETodo(#todo{uid         = Uid,
                priority    = Priority,
                status      = Status,
                doneTime    = DoneTime,
                dueTime     = DueTime,
                createTime  = CreateTime,
                comment     = Comment,
                description = Desc,
                progress    = Progress,
                sharedWith  = SharedWith,
                owner       = Owner}, User, Columns) ->
    ShareText  = makeStr(default(SharedWith, [User])),
    Lists      = doGetLists(User, Uid),
    HasSubTodo = (match(#userInfo{parent = Uid, _ = '_'}) =/= []),
    #etodo{status         = toStr(Status),
           statusCol      = col(?status, Columns),
           statusDB       = Status,
           priority       = toStr(Priority),
           priorityCol    = col(?prio, Columns),
           priorityDB     = Priority,
           owner          = default(Owner, User),
           ownerCol       = col(?owner, Columns),
           dueTime        = toStr(DueTime),
           dueTimeCol     = col(?dueTime, Columns),
           dueTimeDB      = DueTime,
           description    = toStr(Desc),
           descriptionCol = col(?description, Columns),
           comment        = toStr(Comment),
           commentCol     = col(?comment, Columns),
           sharedWith     = ShareText,
           sharedWithCol  = col(?sharedWith, Columns),
           sharedWithDB   = default(SharedWith, [User]),
           createTime     = toStr(CreateTime),
           createTimeCol  = col(?createTime, Columns),
           createTimeDB   = CreateTime,
           doneTime       = toStr(DoneTime),
           doneTimeCol    = col(?doneTimestamp, Columns),
           doneTimeDB     = DoneTime,
           hasSubTodo     = HasSubTodo,
           uid            = toStr(Uid),
           uidCol         = col(?uid, Columns),
           uidDB          = Uid,
           progress       = Progress,
           lists          = makeStr(Lists),
           listsDB        = Lists};
makeETodo(Uid, User, Columns) ->
    case read(todo, Uid) of
        Todo = #todo{} ->
            makeETodo(Todo, User, Columns);
        undefined ->
            eLog:log(error, ?MODULE, makeETodo, [Uid],
                     "No todo for uid... corrupt database?", ?LINE),
            %% Remove object
            mnesia:transaction(
              fun() ->
                      UserInfos = match(#userInfo{uid = Uid, _ = '_'}),
                      [deleteObject(UserInfo) || UserInfo <- UserInfos]
              end),
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
getColumns(User, Columns) ->
    Cols = [{getCol(User, Colum, Def), Desc} || {Colum, Def, Desc} <- Columns],
    lists:keysort(1, Cols).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
filter(_ETodo, []) -> true;
filter(undefined, _Filter) ->
    %% Corrupt database.
    false;
filter(ETodo = #etodo{status = Status, priority = Prio}, Filter) ->
    case filterStatus(Status, Filter) of
        error ->
            eLog:log(debug, ?MODULE, filter, [ETodo, Status],
                     "Unknown status, do not show", ?LINE),
            false;
        true ->
            case filterPrio(Prio, Filter) of
                error ->
                    eLog:log(debug, ?MODULE, filter, [ETodo, Prio],
                             "Unknown priority, do not show", ?LINE),
                    false;
                Else ->
                    Else
            end;
        false ->
            false
    end.

filterStatus(Status, Filter) ->
    case Status of
        ?descPlanning   -> not lists:member(?statusPlanning,   Filter);
        ?descInProgress -> not lists:member(?statusInProgress, Filter);
        ?descDone       -> not lists:member(?statusDone,       Filter);
        ?descNone       -> not lists:member(?statusNone,       Filter);
        _               -> error
    end.

filterPrio(Prio, Filter) ->
    case Prio of
        ?descLow     -> not lists:member(?prioLow,    Filter);
        ?descMedium  -> not lists:member(?prioMedium, Filter);
        ?descHigh    -> not lists:member(?prioHigh,   Filter);
        ?descNone    -> not lists:member(?prioNone,   Filter);
        _            -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
search(_ETodo, _SearchText, []) ->
    false;
search(_, "", _) ->
    true;

search(ETodo, "re~" ++ SearchText, [Col|Rest]) ->
    ColText = getField(Col, ETodo),
    searchRegex(ETodo, SearchText, ColText, Rest);
search(ETodo, "Re~" ++ SearchText, [Col|Rest]) ->
    ColText = getField(Col, ETodo),
    searchRegex(ETodo, SearchText, ColText, Rest);
search(ETodo, "RE~" ++ SearchText, [Col|Rest]) ->
    ColText = getField(Col, ETodo),
    searchRegex(ETodo, SearchText, ColText, Rest);

%% Keep all todos for advanced search
search(_ETodo, "as~" ++ _SearchText, _Cols) ->
    true;
search(_ETodo, "As~" ++ _SearchText, _Cols) ->
    true;
search(_ETodo, "AS~" ++ _SearchText, _Cols) ->
    true;

search(ETodo, SearchText, [Col|Rest]) ->
    ColText = getField(Col, ETodo),
    case catch string:str(string:to_lower(ColText),
                          string:to_lower(SearchText)) of
        0 ->
            search(ETodo, SearchText, Rest);
        _ ->
            true
    end.

searchRegex(ETodo, RegExText, ColText, Rest) ->
    case catch re:run(ColText, RegExText) of
        {match, _} ->
            true;
        _ ->
            search(ETodo, "re~" ++ RegExText, Rest)
    end.

advSearch(User, "as~" ++ SearchText, ETodos) ->
    doAdvSearch(User, SearchText, ETodos);
advSearch(User, "As~" ++ SearchText, ETodos) ->
    doAdvSearch(User, SearchText, ETodos);
advSearch(User, "AS~" ++ SearchText, ETodos) ->
    doAdvSearch(User, SearchText, ETodos);
advSearch(_User, _SearchText, ETodos) ->
    ETodos.

doAdvSearch(User, SearchText, ETodos) ->
    case catch parseAdvSearch(start, SearchText, [], []) of
        {'EXIT', Result} ->
            ePeerEM:sendMsg(system, [User], systemEntry,
                            "Syntax error in search field: " ++ SearchText),
            eLog:log(debug, ?MODULE, parseAdvSearch, [Result],
                     "Syntax error in search field.", ?LINE),
            [];
        {error, Left} ->
            ePeerEM:sendMsg(system, [User], systemEntry,
                            "Syntax error in search field when parsing: " ++
                                Left),
            [];
        TokenizedSearch ->
            executeSearch(User, TokenizedSearch, ETodos)
    end.

executeSearch(User, TokenizedSearch, ETodos) ->
    case catch execSearch(TokenizedSearch, [], ETodos) of
        {'EXIT', Result} ->
            ePeerEM:sendMsg(system, [User], systemEntry,
                            "Search failed..."),
            eLog:log(debug, ?MODULE, execSearch, [Result],
                     "Search failed...", ?LINE),
            [];
        Result ->
            Result
    end.

execSearch([], Result, _ETodos) ->
    Result;

execSearch([$(|TokenizedSearch], [], ETodos) ->
    {Content, Rest} = findMatchingParam(TokenizedSearch),
    Result = execSearch(Content, [], ETodos),
    execSearch(Rest, Result, ETodos);

execSearch(['not', $(|Rest], Result1, ETodos) ->
    {Content, Rest2} = findMatchingParam(Rest),
    Result2 = execSearch(Content, [], ETodos),
    execSearch(Rest2, empty(Result1, ETodos) -- Result2, ETodos);
execSearch(['or', $(|Rest], Result1, ETodos) ->
    {Content, Rest2} = findMatchingParam(Rest),
    Result2 = execSearch(Content, [], ETodos),
    Result3 = sets:union(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest2, sets:to_list(Result3), ETodos);
execSearch(['and', $(|Rest], Result1, ETodos) ->
    {Content, Rest2} = findMatchingParam(Rest),
    Result2 = execSearch(Content, [], ETodos),
    Result3 = sets:intersection(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest2, sets:to_list(Result3), ETodos);

execSearch(['not', 'not'|Rest], Result, ETodos) ->
    execSearch(Rest, Result, ETodos);
execSearch(['or', 'not', {list, LName}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     Lists = ETodo#etodo.listsDB,
                     not lists:member(LName, Lists)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:union(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['or', 'not', {Columns, SearchText}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     not search(ETodo, SearchText, Columns)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:union(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['and', 'not', {list, LName}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     Lists = ETodo#etodo.listsDB,
                     not lists:member(LName, Lists)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:intersection(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['and', 'not', {Columns, SearchText}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     not search(ETodo, SearchText, Columns)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:intersection(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['not', {list, LName}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     Lists = ETodo#etodo.listsDB,
                     lists:member(LName, Lists)
             end,
    Result2 = lists:filter(Filter, ETodos),
    execSearch(Rest, empty(Result1, ETodos) -- Result2, ETodos);
execSearch(['not', {Columns, SearchText}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     search(ETodo, SearchText, Columns)
             end,
    Result2 = lists:filter(Filter, ETodos),
    execSearch(Rest, empty(Result1, ETodos) -- Result2, ETodos);
execSearch(['or', {list, LName}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     Lists = ETodo#etodo.listsDB,
                     lists:member(LName, Lists)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:union(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['or', {Columns, SearchText}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     search(ETodo, SearchText, Columns)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:union(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['and', {list, LName}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     Lists = ETodo#etodo.listsDB,
                     lists:member(LName, Lists)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:intersection(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);
execSearch(['and', {Columns, SearchText}|Rest], Result1, ETodos) ->
    Filter = fun(ETodo) ->
                     search(ETodo, SearchText, Columns)
             end,
    Result2 = lists:filter(Filter, ETodos),
    Result3 = sets:intersection(sets:from_list(Result1), sets:from_list(Result2)),
    execSearch(Rest, sets:to_list(Result3), ETodos);

execSearch([{list, LName}|Rest], [], ETodos) ->
    Filter = fun(ETodo) ->
                     Lists = ETodo#etodo.listsDB,
                     lists:member(LName, Lists)
             end,
    Result = lists:filter(Filter, ETodos),
    execSearch(Rest, Result, ETodos);
execSearch([{Columns, SearchText}|Rest], [], ETodos) ->
    Filter = fun(ETodo) ->
                     search(ETodo, SearchText, Columns)
             end,
    Result = lists:filter(Filter, ETodos),
    execSearch(Rest, Result, ETodos).

empty([], Value) ->
    Value;
empty(Value, _) ->
    Value.

%% Done, reverse result.
parseAdvSearch(_State, "", [], ParsedExpr) ->
    lists:reverse(ParsedExpr);
parseAdvSearch(_State, "", {Type, Token}, ParsedExpr) ->
    Token2 = lists:flatten(Token),
    lists:reverse([{Type, Token2}|ParsedExpr]);

%% Parse List or Name
parseAdvSearch(start, [$@|SearchText], [], ParsedExpr) ->
    parseAdvSearch(list, SearchText, {list, []}, ParsedExpr);
parseAdvSearch(start, [$$|SearchText], [], ParsedExpr) ->
    parseAdvSearch(name, SearchText, {name, []}, ParsedExpr);
parseAdvSearch(start, [${|SearchText], [], ParsedExpr) ->
    parseAdvSearch(search, SearchText, {[], []}, ParsedExpr);
parseAdvSearch(start, [$}|SearchText], [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ParsedExpr);

%% Find single char tokens
parseAdvSearch(start, [Char|SearchText], [], ParsedExpr)
  when (Char == $() or (Char == $)) ->
    parseAdvSearch(start, SearchText, [], [Char|ParsedExpr]);

%% Remove whitespace
parseAdvSearch(start, [Char|SearchText], [], ParsedExpr)
  when (Char == 32) or (Char == 10) or (Char == 9) ->
    parseAdvSearch(start, SearchText, [], ParsedExpr);
parseAdvSearch(search, [Char|SearchText], {[], []}, ParsedExpr)
  when (Char == 32) or (Char == 10) or (Char == 9) ->
    parseAdvSearch(search, SearchText, {[], []}, ParsedExpr);
parseAdvSearch(StateName, [Char|SearchText], {Type, []}, ParsedExpr)
  when ((StateName == searchColumn) or (StateName == searchText))
       and  ((Char == 32) or (Char == 10) or (Char == 9)) ->
    parseAdvSearch(StateName, SearchText, {Type, []}, ParsedExpr);

%% Parse logical operators
parseAdvSearch(start, "and" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['and'|ParsedExpr]);
parseAdvSearch(start, "And" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['and'|ParsedExpr]);
parseAdvSearch(start, "AND" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['and'|ParsedExpr]);

parseAdvSearch(start, "or" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['or'|ParsedExpr]);
parseAdvSearch(start, "Or" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['or'|ParsedExpr]);
parseAdvSearch(start, "OR" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['or'|ParsedExpr]);

parseAdvSearch(start, "not" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['not'|ParsedExpr]);
parseAdvSearch(start, "Not" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['not'|ParsedExpr]);
parseAdvSearch(start, "NOT" ++ SearchText, [], ParsedExpr) ->
    parseAdvSearch(start, SearchText, [], ['not'|ParsedExpr]);

%% Parse Quoted String
parseAdvSearch(qString, [$"|SearchText], {list, Token}, ParsedExpr) ->
    Token2 = checkSubList(lists:flatten(Token)),
    parseAdvSearch(start, SearchText, [], [{list, Token2}|ParsedExpr]);
parseAdvSearch(qString, [$"|SearchText], {Type, Token}, ParsedExpr) ->
    Token2 = lists:flatten(Token),
    parseAdvSearch(start, SearchText, [], [{Type, Token2}|ParsedExpr]);
parseAdvSearch(qString, [$\\, Char|SearchText], {Type, Token}, ParsedExpr) ->
    parseAdvSearch(qString, SearchText, {Type, [Token, $\\, Char]}, ParsedExpr);
parseAdvSearch(qString, [Char|SearchText], {Type, Token}, ParsedExpr) ->
    parseAdvSearch(qString, SearchText, {Type, [Token, Char]}, ParsedExpr);

%% Parse List
parseAdvSearch(list, [$"|SearchText], {list, []}, ParsedExpr) ->
    parseAdvSearch(qString, SearchText, {list, []}, ParsedExpr);

%% Parse Name
parseAdvSearch(name, [$"|SearchText], {name, []}, ParsedExpr) ->
    parseAdvSearch(qString, SearchText, {name, []}, ParsedExpr);

%% Parse normal search
parseAdvSearch(search, [$[|SearchText], {[], []}, ParsedExpr) ->
    parseAdvSearch(searchColumn, SearchText, {[], []}, ParsedExpr);

parseAdvSearch(searchColumn, [$,|SearchText], {Type, Token}, ParsedExpr) ->
    Token2 = eTodoUtils:taskInternal(lists:flatten(Token)),
    parseAdvSearch(searchColumn, SearchText, {[Token2|Type], []}, ParsedExpr);
parseAdvSearch(searchColumn, [$]|SearchText], {Type, Token}, ParsedExpr) ->
    Token2 = eTodoUtils:taskInternal(lists:flatten(Token)),
    parseAdvSearch(searchText, SearchText, {[Token2|Type], []}, ParsedExpr);
parseAdvSearch(searchColumn, [Char|SearchText], {Type, Token}, ParsedExpr) ->
    parseAdvSearch(searchColumn, SearchText, {Type, [Token, Char]}, ParsedExpr);

parseAdvSearch(searchText, [$,|SearchText], {Type, []}, ParsedExpr) ->
    parseAdvSearch(searchText, SearchText, {Type, []}, ParsedExpr);
parseAdvSearch(searchText, [$"|SearchText], {Type, []}, ParsedExpr) ->
    parseAdvSearch(qString, SearchText, {Type, []}, ParsedExpr);

parseAdvSearch(_State, SearchText, _Token, _ParsedExpr) ->
    {error, SearchText}.

findMatchingParam(TokenizedSearch) ->
    findMatchingParam(TokenizedSearch, [], []).

findMatchingParam([$)|Rest], Content, []) ->
    {lists:reverse(Content), Rest};
findMatchingParam([$(|Rest], Content, Params) ->
    findMatchingParam(Rest, [$(|Content], [$)|Params]);
findMatchingParam([$)|Rest], Content, Params) ->
    findMatchingParam(Rest, [$)|Content], Params -- [$)]);
findMatchingParam([Char|Rest], Content, Params) ->
    findMatchingParam(Rest, [Char|Content], Params).

%% Sublists have integer names.
checkSubList(Value) when is_list(Value) ->
    case catch list_to_integer(Value) of
        {'EXIT', _} ->
            Value;
        Value2 ->
            Value2
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
sort(User, Result) ->
    Config  = readConfig(listCfg, User, sorted),
    Sorting = default(getField(listCfg, sorted, Config), default),
    sortResult(Result, Sorting).

sortResult(Result, default) ->
    Result;
sortResult(Result, {ascending, ColName}) ->
    lists:keysort(sortCol(ColName), Result);
sortResult(Result, {descending, ColName}) ->
    lists:reverse(lists:keysort(sortCol(ColName), Result)).

sortCol(?uid)           -> #etodo.uidDB;
sortCol(?status)        -> #etodo.status;
sortCol(?prio)          -> #etodo.priority;
sortCol(?dueTime)       -> #etodo.dueTime;
sortCol(?description)   -> #etodo.description;
sortCol(?comment)       -> #etodo.comment;
sortCol(?sharedWith)    -> #etodo.sharedWith;
sortCol(?createTime)    -> #etodo.createTime;
sortCol(?doneTimestamp) -> #etodo.doneTime;
sortCol(?owner)         -> #etodo.owner.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
doMoveTodosToTaskList(_User, _List, []) ->
    ok;
doMoveTodosToTaskList(User, ToList, [List|Rest]) ->
    MoveList = match(#userInfo{userName = User, parent = List, _ = '_'}),
    [moveTodoToTaskList(Object, ToList) || Object <- MoveList],
    doMoveTodosToTaskList(User, ToList, Rest).

moveTodoToTaskList(Object, ?defTaskList) ->
    %% TASK already present in ?defTaskList, just remove it.
    mnesia:transaction(
      fun() ->
              delete_object(Object)
      end);
moveTodoToTaskList(Object, ToList) ->
    mnesia:transaction(
      fun() ->
              delete_object(Object),
              mnesia:write(Object#userInfo{parent = ToList})
      end).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
doAssignLists(User, Uid, Lists) ->
    OldUserInfo = match(#userInfo{userName = User, uid = Uid, _ = '_'}),
    %% Keep assignments for lists that hasn't been removed.
    {Remove, Add} = modifyAssignments(User, Uid, OldUserInfo, Lists, {[], []}),
    {mnesia:transaction(
       fun() ->
               [deleteObject(UserInfo) || UserInfo <- Remove],
               [mnesia:write(UserInfo) || UserInfo <- Add]
       end),
     OldUserInfo}.

undoAssignLists(User, Uid, UndoInfo) ->
    OldUserInfo = match(#userInfo{userName = User, uid = Uid, _ = '_'}),
    mnesia:transaction(
      fun() ->
              [deleteObject(UserInfo) || UserInfo <- OldUserInfo],
              [mnesia:write(UserInfo) || UserInfo <- UndoInfo]
      end).

modifyAssignments(_User, _Uid, [], [], Acc) ->
    Acc;
modifyAssignments(User, Uid, [], [NewList|Rest], {Remove, Add}) ->
    Row = doGetRow(User, NewList),
    UserInfo = #userInfo{userName = User,
                         uid      = Uid,
                         parent   = NewList,
                         row      = Row},
    modifyAssignments(User, Uid, [], Rest, {Remove, [UserInfo|Add]});
modifyAssignments(User, Uid, [UserInfo|UserInfos], Lists, {Rem, Add}) ->
    List = UserInfo#userInfo.parent,
    case lists:member(List, Lists) of
        true ->
            Lists2 = Lists -- [List],
            modifyAssignments(User, Uid, UserInfos, Lists2, {Rem, Add});
        false ->
            Rem2 = [UserInfo|Rem],
            modifyAssignments(User, Uid, UserInfos, Lists, {Rem2, Add})
    end.

doGetRow(User, NewList) ->
    List = match(#userInfo{userName = User, parent = NewList, _ = '_'}),
    lists:max([-1|[RowNr || #userInfo{row = RowNr} <- List]]) + 1.

getTodosSharedWith(User1, User2, Result) ->
    getTodosSharedWith(User1, User2, Result, []).

getTodosSharedWith(_User1, _User2, [], Acc) ->
    lists:keysort(#todo.uid, Acc);
getTodosSharedWith(User1, User2, [Todo|Rest], Acc) ->
    case lists:member(User1, default(Todo#todo.sharedWith, [])) of
        true ->
            case lists:member(User2, default(Todo#todo.sharedWith, [])) of
                true ->
                    getTodosSharedWith(User1, User2, Rest, [Todo | Acc]);
                false ->
                    getTodosSharedWith(User1, User2, Rest, Acc)
            end;
        false ->
            getTodosSharedWith(User1, User2, Rest, Acc)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
doGetLists(User, Uid) ->
    UserInfos = match(#userInfo{userName = User, uid = Uid, _ = '_'}),
    case UserInfos of
        [] ->
            [?defTaskList]; %% This is a newly made task.
        UserInfos ->
            lists:sort([toStr(List) || #userInfo{parent = List} <- UserInfos])
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
calcDiff(NewTodo = #todo{sharedWith = Shares},
         OldTodo = #todo{sharedWith = Shares}) ->
    Diff = calcDiffs(NewTodo, OldTodo,
                     [#todo.priority,
                      #todo.status,
                      #todo.doneTime,
                      #todo.dueTime,
                      #todo.createTime,
                      #todo.description,
                      #todo.comment,
                      #todo.progress,
                      #todo.owner], []),
    #diff{uid = NewTodo#todo.uid, diff = Diff};
calcDiff(NewTodo, _OldTodo) ->
    %% Share complete task when shares have been modified or old task undefined.
    NewTodo.

calcDiffs(_NewTodo, _OldTodo, [], Diff) ->
    Diff;
calcDiffs(NewTodo, OldTodo, [Element|Rest], Diff) ->
    Diff2 = calcDiff(element(Element, NewTodo),
                     element(Element, OldTodo),
                     Element, Diff),
    calcDiffs(NewTodo, OldTodo, Rest, Diff2).

calcDiff(Value,  Value,  _Element, Diff) -> Diff;
calcDiff(Value1, Value2,  Element, Diff) when is_list(Value1),
                                              is_list(Value2) ->
    [{Element, calcStringDiff(Value1, Value2)} | Diff];
calcDiff(Value1, _Value2, Element, Diff) -> [{Element, Value1} | Diff].

calcStringDiff(New, Old) ->
    RNew = lists:reverse(New),
    ROld = lists:reverse(Old),
    calcStringDiff(New, Old, 0, RNew, ROld, 0, length(New), length(Old)).

calcStringDiff(Str,  [], Pos, _RStr, [], RPos, LenNew, LenOld)
  when Pos  =/= 0, RPos =/= 0, ((LenNew - LenOld) > 0) ->
    %% Text has been added in the middle of the string.

    %% Make sure no overlap occurred.
    RPos2 = case (Pos >= (LenOld - RPos)) of
                true ->
                    %% Overlap when parsing old content.
                    LenOld - Pos;
                false ->
                    RPos
            end,

    {sub, Pos, string:substr(Str, 1, length(Str) - RPos2)};
calcStringDiff(_Str,  [], Pos, _RStr, [], RPos, LenNew, LenOld)
  when Pos  =/= 0, RPos =/= 0 ->
    %% Text has been removed in the middle of the string.
    {sub, Pos, LenNew - LenOld};
calcStringDiff(Str1,  [], Pos, _RStr1, [], RPos, _, _) when Pos >= RPos ->
    {Pos, Str1};
calcStringDiff(_Str1, [], Pos, RStr1,  [], RPos, _, _) when RPos > Pos  ->
    {rev, RPos, RStr1};
calcStringDiff([Char|Str1],   [Char|Str2],   Pos,
               [RChar|RStr1], [RChar|RStr2], RPos, New, Old) ->
    calcStringDiff(Str1, Str2, Pos + 1, RStr1, RStr2, RPos + 1, New, Old);
calcStringDiff([Char|Str1], [Char|Str2], Pos, RStr1, _RStr2, RPos, New, Old) ->
    calcStringDiff(Str1, Str2, Pos + 1, RStr1, [], RPos, New, Old);
calcStringDiff(Str1, _Str2, Pos, [RChar|RStr1], [RChar|RStr2], RPos, New, Old) ->
    calcStringDiff(Str1, [], Pos, RStr1, RStr2, RPos + 1, New, Old);
calcStringDiff(Str1,  _Str2,  Pos, RStr1, _RStr2, RPos, New, Old) ->
    calcStringDiff(Str1, [], Pos, RStr1, [], RPos, New, Old).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
applyDiff(#diff{diff = []}, Todo) ->
    Todo;
applyDiff(Diff = #diff{diff = [{Element, {Pos, Add}} | Rest]}, Todo)
  when is_integer(Pos), is_list(Rest) ->
    OldValue = element(Element, Todo),
    NewValue = string:substr(OldValue, 1, Pos) ++ Add,
    applyDiff(Diff#diff{diff = Rest}, setelement(Element, Todo, NewValue));
applyDiff(Diff = #diff{diff = [{Element, {rev, Pos, Add}} | Rest]}, Todo)
  when is_integer(Pos), is_list(Rest) ->
    OldValue = lists:reverse(element(Element, Todo)),
    NewValue = lists:reverse(string:substr(OldValue, 1, Pos) ++ Add),
    applyDiff(Diff#diff{diff = Rest}, setelement(Element, Todo, NewValue));
applyDiff(Diff = #diff{diff = [{Element, {sub, Pos, Len}} | Rest]}, Todo)
  when is_integer(Pos), is_list(Rest), is_integer(Len) ->
    OldValue = element(Element, Todo),
    NewValue = string:substr(OldValue, 1, Pos) ++
        string:substr(OldValue, 1 + Pos - Len),
    applyDiff(Diff#diff{diff = Rest}, setelement(Element, Todo, NewValue));
applyDiff(Diff = #diff{diff = [{Element, {sub, Pos, Str}} | Rest]}, Todo)
  when is_integer(Pos), is_list(Rest), is_list(Str) ->
    OldValue = element(Element, Todo),
    NewValue = string:substr(OldValue, 1, Pos) ++ Str ++
        string:substr(OldValue, 1 + Pos),
    applyDiff(Diff#diff{diff = Rest}, setelement(Element, Todo, NewValue));
applyDiff(Diff = #diff{diff = [{Element, NewValue} | Rest]}, Todo) ->
    applyDiff(Diff#diff{diff = Rest}, setelement(Element, Todo, NewValue)).

%%--------------------------------------------------------------------
%% @doc
%% Summarize logged work
%% @spec
%% @end
%%--------------------------------------------------------------------
tot(WorkedTime) ->
    tot(WorkedTime, {0, 0}).

tot([], {SumHours, SumMin}) ->
    SumHours2   = SumHours + (SumMin div 60),
    SumMinutes2 = SumMin rem 60,
    time(SumHours2) ++ ":" ++ time(SumMinutes2);
tot([#logWork{hours = Hours, minutes = Min}|Rest], {SumHours, SumMin}) ->
    tot(Rest, {SumHours + Hours, SumMin + Min}).

time(Min) when Min < 10 ->
    "0" ++ integer_to_list(Min);
time(Min) ->
    integer_to_list(Min).

%%--------------------------------------------------------------------
%% @doc
%% Summarize logged work
%% @spec
%% @end
%%--------------------------------------------------------------------
totInt(WorkedTime) ->
    totInt(WorkedTime, {0, 0}).

totInt([], {SumHours, SumMin}) ->
    SumHours2   = SumHours + (SumMin div 60),
    SumMinutes2 = SumMin rem 60,
    {SumHours2,  SumMinutes2};
totInt([#logWork{hours = Hours, minutes = Min}|Rest], {SumHours, SumMin}) ->
    totInt(Rest, {SumHours + Hours, SumMin + Min}).

%%--------------------------------------------------------------------
%% @doc
%% Get Work log data for task.
%% @spec
%% @end
%%--------------------------------------------------------------------
showInWorkLog(LW) ->
    WorkDesc = default(matchOne(#workDesc{uid = LW#logWork.uid, _ = '_'}),
                       #workDesc{}),
    case  WorkDesc#workDesc.showInWorkLog == true of
        true ->
            true;
        false ->
            ({LW#logWork.hours, LW#logWork.minutes} =/= {0, 0}) and
                                                                  (LW#logWork.hours =/= undefined) and
                                                                                                     (LW#logWork.minutes =/= undefined)
    end.

addToLoggedWork(LoggedWork, ShowInWL) ->
    EmptyLW = [#logWork{uid = WD#workDesc.uid, hours = 0, minutes = 0} ||
                  WD <-  ShowInWL],
    updateEmptyLW(LoggedWork, EmptyLW).

updateEmptyLW([], SoFar) ->
    SoFar;
updateEmptyLW([LW|Rest], SoFar) ->
    updateEmptyLW(Rest, lists:keystore(LW#logWork.uid, #logWork.uid, SoFar, LW)).
