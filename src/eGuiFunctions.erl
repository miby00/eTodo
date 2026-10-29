%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 4 July 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eGuiFunctions).
-author("mikael.bylund@gmail.com").

-include("../include/eTodo.hrl").

-include_lib("wx/include/wx.hrl").

%% API
-export([
         addIfEmail/3,
         addTodo/4,
         appendToPage/4,
         appendToPage/6,
         chatMsgStatusBar/3,
         checkStatus/1,
         checkUndoStatus/1,
         clearAndInitiate/2,
         clearMsgCounter/1,
         showTaskDetails/2,
         msgKindShown/2,
         reloadStaleMessages/1,
         clearNotificationTimer/1,
         clearStatusBar/1,
         convertToLocal/1,
         date2wxDate/1,
         deleteAndUpdate/3,
         doLogout/2,
         eTodoUpdate/2,
         filterAndConvert2Utf8/1,
         filterAndConvert2Utf8Text/1,
         findSelected/1,
         focusAndSelect/1,
         focusAndSelect/2,
         generateSchedule/1,
         refreshVisibleReport/1,
         generateTimeLog/1,
         generateWorkLog/1,
         getCheckedItems/1,
         getNotificationTime/1,
         getPortrait/1,
         getTaskList/1,
         getTodoList/2,
         getTodoLists/1,
         getUsersWithEmail/1,
         getWorkDesc/2,
         makeETodo/3,
         obj/2,
         pos/2,
         saveColumnSizes/1,
         saveMsg/2,
         setColor/2,
         setColumnWidth/4,
         setDoneTimeStamp/3,
         setMsgCounter/2,
         markMessagesRead/1,
         setOwner/3,
         setPeerStatusIfNeeded/1,
         setPortrait/2,
         setSelection/1,
         setSelection/2,
         setSelection/4,
         setTaskLists/2,
         setText/2,
         setUserStatus/2,
         setUnreadMsgs/2,
         showBookmarkMenu/2,
         showMenu/2,
         showMenu/4,
         toClipboard/1,
         toClipboard/2,
         type/1,
         updateGui/3,
         updateGui/4,
         fillEmailCheckBox/1,
         updateMsgWindow/2,
         updateTodo/4,
         updateTodoInDB/2,
         updateTodoWindow/1,
         updateValue/4,
         useFilter/3,
         userStatusAvailable/1,
         userStatusAway/1,
         userStatusBusy/1,
         userStatusOffline/1,
         userStatusUpdate/1,
         wxDate2Date/1,
         xrcId/1
        ]).

-import(eTodoUtils, [col/2,
                     dateTime/0,
                     default/2,
                     doneTime/2,
                     getRootDir/0,
                     makeStr/1,
                     toStr/1]).

-import(eRows, [findIndex/2,
                getETodoAtIndex/2,
                insertRow/3,
                updateRows/2]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
addTodo(TodoList, ETodo, Row, State) ->
    wxListCtrl:insertItem(TodoList, Row, ""),
    Rows2 = insertRow(ETodo, Row, State#guiState.rows),
    updateTodo(TodoList, ETodo, Row, State#guiState{rows = Rows2}).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateTodo(List, ETodo, Row, State) ->
    setRowInfo(List, ETodo, Row),
    Rows2 = updateRows(ETodo, State#guiState.rows),
    State#guiState{rows = Rows2}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
appendToPage(MsgObj, Type, {Html, _HtmlCSS}, State) ->
    appendToPage(MsgObj, Type, "", "", {Html, _HtmlCSS}, State).

%% The Messages view reloads its timelines when eTodoDB pushes the change.
appendToPage(_MsgObj, Type, From, To, {Html, _HtmlCSS}, State) ->
    eTodoDB:appendToPage(State#guiState.user, Type, From, To, Html),
    case evalShow(Type, From, To, State#guiState.msgCfg) orelse
        not chatVisible(State) of
        true ->
            State;
        false ->
            %% Hidden by the chat filter while Chat is open; chatMsgStatusBar
            %% only counts messages arriving on a kind that isn't shown.
            State2 = increaseMsgCounter(State, obj("mainNotebook", State)),
            saveMsgCounters(increaseKindCounter(Type, State2))
    end.

%% The peer filter only applies to chat; reminders and system messages
%% are always shown.
evalShow(msgEntry, UserName, To, {true, Users}) ->
    lists:member(UserName, Users) or (Users =/= (Users -- To));
evalShow(_,  _From, _To, _msgCfg)       -> true.

chatVisible(State) ->
    messagesVisible(State) andalso msgKind() =:= msgEntry.

messagesVisible(State) ->
    Notebook = obj("mainNotebook", State),
    wxNotebook:getCurrentPage(Notebook) =:= wxNotebook:getPage(Notebook, 1).

%% Message kind shown in the Messages view, reported by its page script.
msgKind() ->
    case get(msgKind) of
        undefined -> msgEntry;
        Kind      -> Kind
    end.

msgKindShown(Kind, State) ->
    put(msgKind, Kind),
    case messagesVisible(State) of
        true  -> clearMsgCounter(State);
        false -> State
    end.

%% Hiding the details gives the task list the whole height.
showTaskDetails(Show, Frame) ->
    wxWindow:show(obj("taskEditPanel", Frame), [{show, Show}]),
    wxMenuBar:check(wxFrame:getMenuBar(Frame),
                    xrcId("toggleDetailsMenuItem"), Show),
    wxWindow:layout(obj("taskSplitPanel", Frame)).

loadMessages(View, #guiState{user = User, msgCfg = Cfg}) ->
    put(msgViewLoaded, erlang:monotonic_time(second)),
    eWorkLogView:load_messages(View, User, kindName(msgKind()), Cfg).

kindName(msgEntry)   -> "chat";
kindName(alarmEntry) -> "reminders";
kindName(_System)    -> "system".

%% The page keeps a desktop session; renew it well before it expires.
reloadStaleMessages(State) ->
    Loaded = get(msgViewLoaded),
    case is_integer(Loaded) andalso
        erlang:monotonic_time(second) - Loaded < 3600 of
        true  -> State;
        false -> updateMsgWindow(State, State#guiState.user)
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
doLogout(_User, State = #guiState{loggedIn = false}) ->
    State;
doLogout(User, State) ->
    eWorkLogView:revoke(),
    StatusBar = obj("mainStatusBar", State),
    wxStatusBar:setStatusText(StatusBar, User,           [{number, 0}]),
    wxStatusBar:setStatusText(StatusBar, ?sbNotLoggedIn, [{number, 1}]),
    userStatusOffline(State),

    ePeerEM:loggedOut(User),
    case application:get_env(eTodo, user) of
        {ok, User} -> application:unset_env(eTodo, user);
        _ -> ok
    end,
    checkStatus(State#guiState{loggedIn = false}).


%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getCheckedItems(Obj) ->
    Count = wxCheckListBox:getCount(Obj),
    getCheckedItems(Obj, Count - 1, []).

getCheckedItems(_Obj, -1, Items) ->
    lists:sort(Items);
getCheckedItems(Obj, Index, Items) ->
    case wxCheckListBox:isChecked(Obj, Index) of
        true ->
            Item = wxCheckListBox:getString(Obj, Index),
            case catch list_to_integer(Item) of
                {'EXIT', _Reason} ->
                    getCheckedItems(Obj, Index - 1, [Item|Items]);
                IntValue ->
                    getCheckedItems(Obj, Index - 1, [IntValue|Items])
            end;
        false ->
            getCheckedItems(Obj, Index - 1, Items)
    end.

%%======================================================================
%% Get configured task list
%%======================================================================
getTaskList(State = #guiState{drillDown = []}) ->
    Obj   = obj("taskListChoice", State),
    Index = wxChoice:getSelection(Obj),
    wxChoice:getString(Obj, Index);
getTaskList(#guiState{drillDown = [Uid|_Rest]}) ->
    Uid.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getTodoList(_List, State) -> obj("mainTaskList", State).

%%======================================================================
%% Function : obj(Name, State) -> This
%% Purpose  : Get object reference for object Name...
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getTodoLists(User) ->
    UserCfg = eTodoDB:readUserCfg(User),
    lists:sort(default(UserCfg#userCfg.lists, [?defTaskList])).

%%======================================================================
%% Function : obj(Name, State) -> This
%% Purpose  : Get object reference for object Name...
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
focusAndSelect(State) ->
    focusAndSelect(0, State).

focusAndSelect(Index, State) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    wxListCtrl:setFocus(TodoList),
    case wxListCtrl:getItemCount(TodoList) of
        Row when (Row > Index) and (Index >= 0) ->
            wxListCtrl:ensureVisible(TodoList, Index),
            wxListCtrl:setItemState(TodoList, Index,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED);
        Row when (Row == Index) and (Index > 0) ->
            wxListCtrl:ensureVisible(TodoList, Index - 1),
            wxListCtrl:setItemState(TodoList, Index - 1,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED);
        Row when Row > 0 ->
            wxListCtrl:ensureVisible(TodoList, 0),
            wxListCtrl:setItemState(TodoList, 0,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED);
        _Row ->
            updateGui(undefined, 0, State)
    end,
    State.
%%======================================================================
%% Function : obj(Name, State) -> This
%% Purpose  : Get object reference for object Name...
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
obj(Name, #guiState{frame = Frame}) ->
    obj(Name, Frame);
obj(Name, Frame) ->
    %% Cache for faster access.
    case get({wxObj, Name}) of
        undefined ->
            case findObject(Frame, Name) of
                {wx_ref, 0, Type, _Args} ->
                    eLog:log(error, ?MODULE, obj,
                             [Frame, Name, Type],
                             "Object not found.", ?LINE),
                    undefined;
                Obj ->
                    eLog:log(debug, ?MODULE, obj,
                             [Frame, Name, Obj],
                             "Object not found in cache, cache object", ?LINE),
                    put({wxObj, Name}, Obj),
                    Obj
            end;
        Obj ->
            Obj
    end.

findObject(Frame, Name) ->
    case wxXmlResource:xrcctrl(Frame, Name, type(Name)) of
        {wx_ref, 0, _, _} = Missing ->
            case get(taskDetailsDialog) of
                undefined -> Missing;
                Dialog -> wxXmlResource:xrcctrl(Dialog, Name, type(Name))
            end;
        Obj ->
            Obj
    end.

type(Component) ->
    maps:get(Component, ?wxTypes, {error, {missing, Component}}).


%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
setDoneTimeStamp(ETodo = #etodo{statusDB = Status}, Index,
                 State = #guiState{user     = User}) ->
    OldDoneTime = ETodo#etodo.doneTimeDB,
    DoneTime    = doneTime(OldDoneTime, Status),
    ETodo2      = ETodo#etodo{doneTimeDB = DoneTime,
                              doneTime   = toStr(DoneTime)},

    updateTodoInDB(User, ETodo2),

    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    State2   = updateTodo(TodoList, ETodo2, Index, State),
    eLog:log(debug, ?MODULE, saveGuiSettings, [ETodo2],
             "Active todo updated.", ?LINE),
    State2#guiState{activeTodo = {ETodo2, Index}}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
setTaskLists(Lists, State = #guiState{user = User}) ->
    Obj2 = obj("taskListChoice", State),
    TodoLists = getTodoLists(User),
    eTodoDB:moveTodosToTaskList(User, ?defTaskList, TodoLists -- Lists),
    UserCfg = eTodoDB:readUserCfg(User),
    eTodoDB:saveUserCfg(UserCfg#userCfg{lists = Lists}),
    TaskList = getTaskList(State),
    wxChoice:clear(Obj2),
    Default =
        case TaskList of
            ?defTaskList ->
                default(UserCfg#userCfg.lastTaskList, ?defTaskList);
            Uid when is_integer(Uid) ->
                SubTaskText = ?subTaskList ++ toStr(Uid),
                wxChoice:append(Obj2, SubTaskText),
                SubTaskText;
            Chosen ->
                Chosen
        end,
    wxChoice:append(Obj2, ?defInbox),
    [wxChoice:append(Obj2, List) || List <- Lists],
    setSelection(Obj2, Default).

setSelection(Obj) ->
    setSelection(Obj, ?defTaskList).

setSelection(Obj, Selection) ->
    Count = wxChoice:getCount(Obj),
    setSelection(Obj, Count - 1, Selection).

setSelection(Obj, 0, _Selection) ->
    wxChoice:setSelection(Obj, 0);
setSelection(Obj, Index, Selection) ->
    case wxChoice:getString(Obj, Index) of
        Selection ->
            wxChoice:setSelection(Obj, Index);
        ?descNA when Selection == "" ->
            wxChoice:setSelection(Obj, Index);
        _ ->
            setSelection(Obj, Index - 1, Selection)
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateGui(undefined, _Index, State = #guiState{user    = User,
                                               columns = Columns}) ->
    updateGui(makeETodo(#todo{}, User, Columns), 0, State);
updateGui(ETodo, Index, State) ->
    updateValue("descriptionAreaSTC",      State, ETodo#etodo.description),
    updateValue("sharedWithText",          State, ETodo#etodo.sharedWith),
    updateValue("addedToLists",            State, ETodo#etodo.lists),
    updateValue("commentAreaSTC",          State, ETodo#etodo.comment),
    doSetSelection("statusChoice",         State, ETodo#etodo.status),
    doSetSelection("priorityChoice",       State, ETodo#etodo.priority),
    DueDateObj      = obj("dueDatePicker", State),
    DueDateUsedObj  = obj("dueDateUsed",   State),
    ProgressObj     = obj("progressInfo",  State),
    OwnerObj        = obj("ownerChoice",   State),

    case ETodo#etodo.dueTimeDB of
        undefined ->
            wxCheckBox:setValue(DueDateUsedObj, false),
            wxDatePickerCtrl:setValue(DueDateObj, date2wxDate(undefined));
        DueTimeDB ->
            wxCheckBox:setValue(DueDateUsedObj, true),
            wxDatePickerCtrl:setValue(DueDateObj, date2wxDate(DueTimeDB))
    end,
    setOwner(OwnerObj, State#guiState.user, ETodo#etodo.owner),


    case ETodo#etodo.statusDB of
        done ->
            wxSpinCtrl:setValue(ProgressObj, 100);
        _ ->
            wxSpinCtrl:setValue(ProgressObj, default(ETodo#etodo.progress, 0))
    end,

    %% Update preview view if shown.
    eGuiEvents:commentNotebookEvent(undefined, undefined, undefined, State),
    eGuiEvents:descNotebookEvent(undefined, undefined, undefined, State),

    checkStatus(setDoneTimeStamp(ETodo, Index, State)).

updateValue(_Name, _State, Value, Value) ->
    ok;
updateValue(Name, State, Value, _OldValue) ->
    updateValue(Name, State, Value).

updateValue(Name, State, Value) ->
    Obj = obj(Name, State),
    case {type(Name), Name} of
        {wxTextCtrl, Name} ->
            wxTextCtrl:setValue(Obj, Value);
        {wxStyledTextCtrl, Name} ->
            setText(Obj, Value);
        {wxStaticText, _} ->
            wxStaticText:setLabel(Obj, Value)
    end.

setSelection(_Name, _State, Value, Value) ->
    ok;
setSelection(Name, State, Value, _OldValue) ->
    doSetSelection(Name, State, Value).

doSetSelection(Name, State, Value) ->
    Obj = obj(Name, State),
    setSelection(Obj, Value).

setOwner(OwnerObj, User, Owner) ->
    Peers    = eTodoDB:getUsers(),
    #userCfg{ownerCfg = OwnerCfg} = eTodoDB:readUserCfg(User),
    PeerList = [User | lists:delete(User, Peers)] ++ default(OwnerCfg, []),
    wxChoice:clear(OwnerObj),
    [wxChoice:append(OwnerObj, Peer) || Peer <- lists:sort(PeerList)],
    setSelection(OwnerObj, Owner).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateGui(#etodo{description = OldDesc,
                 sharedWith  = OldShare,
                 lists       = OldLists,
                 comment     = OldComment,
                 status      = OldStatus,
                 priority    = OldPriority,
                 dueTimeDB   = OldDueTime,
                 owner       = OldOwner},
          ETodo = #etodo{description = Desc,
                         sharedWith  = Share,
                         lists       = Lists,
                         comment     = Comment,
                         status      = Status,
                         priority    = Priority,
                         dueTimeDB   = DueTime,
                         owner       = Owner,
                         progress    = Progress}, Index, State) ->

    updateValue("descriptionAreaSTC",  State, Desc,     OldDesc),
    updateValue("sharedWithText",      State, Share,    OldShare),
    updateValue("addedToLists",        State, Lists,    OldLists),
    updateValue("commentAreaSTC",      State, Comment,  OldComment),
    setSelection("statusChoice",       State, Status,   OldStatus),
    setSelection("priorityChoice",     State, Priority, OldPriority),
    DueDateObj  = obj("dueDatePicker", State),
    if DueTime =/= OldDueTime ->
            wxDatePickerCtrl:setValue(DueDateObj, date2wxDate(DueTime));
       true ->
            ok
    end,
    if Owner =/= OldOwner ->
            OwnerObj = obj("ownerChoice",   State),
            sendUpdate(State#guiState.user, Owner, ETodo),
            setOwner(OwnerObj, State#guiState.user, Owner);
       true ->
            ok
    end,
    ProgressObj = obj("progressInfo",  State),

    case ETodo#etodo.statusDB of
        done ->
            wxSpinCtrl:setValue(ProgressObj, 100);
        _ ->
            wxSpinCtrl:setValue(ProgressObj, default(Progress, 0))
    end,
    setDoneTimeStamp(ETodo, Index, State).

setRowInfo(List, ETodo, Row) ->
    Count = wxListCtrl:getItemCount(List),
    setRowInfo(List, ETodo, Row, Count).

setRowInfo(List, #etodo{priority       = Priority,
                        priorityCol    = PriorityCol,
                        status         = Status,
                        statusCol      = StatusCol,
                        owner          = Owner,
                        ownerCol       = OwnerCol,
                        doneTime       = DoneTime,
                        doneTimeCol    = DoneTimeCol,
                        dueTime        = DueTime,
                        dueTimeCol     = DueTimeCol,
                        createTime     = CreateTime,
                        createTimeCol  = CreateTimeCol,
                        comment        = Comment,
                        commentCol     = CommentCol,
                        description    = Desc,
                        descriptionCol = DescCol,
                        sharedWith     = SharedWith,
                        sharedWithCol  = SharedWithCol,
                        uid            = Uid,
                        uidCol         = UidCol,
                        hasSubTodo     = HasSubTodo}, Row, Count) ->

    case Count > 0 of
        true ->
            setItem(List, Row, UidCol,        Uid),
            setItem(List, Row, StatusCol,     Status),
            setItem(List, Row, OwnerCol,      Owner),
            setItem(List, Row, PriorityCol,   Priority),
            setItem(List, Row, DueTimeCol,    DueTime),
            setItem(List, Row, DescCol,       colFormat(Desc)),
            setItem(List, Row, CommentCol,    colFormat(Comment)),
            setItem(List, Row, SharedWithCol, SharedWith),
            setItem(List, Row, CreateTimeCol, CreateTime),
            setItem(List, Row, DoneTimeCol,   DoneTime),

            case HasSubTodo of
                false ->
                    case Status of
                        ?descDone ->
                            wxListCtrl:setItemImage(List, Row, 2);
                        _ ->
                            wxListCtrl:setItemImage(List, Row, 0)
                    end;
                true ->
                    wxListCtrl:setItemImage(List, Row, 1)
            end,
            setColor(List, Row);
        false ->
            ok
    end.

setColor(List, Row) ->
    {BR, BG, BB, _} = wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOX),
    Text = wxSystemSettings:getColour(?wxSYS_COLOUR_LISTBOXTEXT),
    Back = case Row rem 2 of
               0 -> {BR, BG, BB, 255};
               _ -> {Rr, Gg, Bb, _} = Text,
                    {shade(BR, Rr), shade(BG, Gg), shade(BB, Bb), 255}
           end,
    wxListCtrl:setItemBackgroundColour(List, Row, Back),
    wxListCtrl:setItemTextColour(List, Row, Text).

%% Move a background channel 6% towards the text colour for striping.
shade(Back, Text) ->
    Back + round((Text - Back) * 0.06).

colFormat(ColumnText) ->
    colFormat(ColumnText, 160, []).

colFormat([], _Num, Text) ->
    lists:reverse(Text);
colFormat(_ColumnText, 0, Text) ->
    lists:reverse("..." ++ Text);
colFormat([13, 10, 13, 10|_Rest], _Num, Text) ->
    lists:reverse(Text);
colFormat([10, 10|_Rest], _Num, Text) ->
    lists:reverse(Text);
colFormat([10|Rest], Num, Text) ->
    colFormat(Rest, Num - 1, [32|Text]);
colFormat([13|Rest], Num, Text) ->
    colFormat(Rest, Num - 1, [32|Text]);
colFormat([Char|Rest], Num, Text) ->
    colFormat(Rest, Num - 1, [Char|Text]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateTodoInDB(_User, #etodo{uidDB = undefined}) ->
    %% This is a constructed task for clearing task window, do not save.
    ok;
updateTodoInDB(User, #etodo{uidDB        = Uid,
                            priorityDB   = Prio,
                            statusDB     = Status,
                            doneTimeDB   = DoneTime,
                            dueTimeDB    = DueTime,
                            createTimeDB = CreateTime,
                            comment      = Comment,
                            description  = Description,
                            progress     = Progress,
                            sharedWithDB = SharedWith,
                            owner        = Owner}) ->
    Todo = #todo{uid         = Uid,
                 priority    = Prio,
                 status      = Status,
                 doneTime    = DoneTime,
                 dueTime     = DueTime,
                 createTime  = CreateTime,
                 comment     = Comment,
                 description = Description,
                 progress    = Progress,
                 sharedWith  = SharedWith,
                 owner       = Owner},
    eTodoDB:updateTodo(User, Todo).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateTodoWindow(State = #guiState{searchCfg = Cfg,
                                   user      = User,
                                   filter    = Filter}) ->
    SearchText = wxComboBox:getValue(obj("searchText", State)),
    TaskList   = getTaskList(State),
    TodoList   = getTodoList(TaskList, State),
    TodoLists  = getTodoLists(User),
    setTaskLists(TodoLists, State),

    TaskList3 = case TaskList of
                    ?subTaskList ++ _ ->
                        TaskList2 = State#guiState.drillFromList,
                        setSelection(obj("taskListChoice", State), TaskList2),
                        TaskList2;
                    _ ->
                        TaskList
                end,
    Filter2 = useFilter(TaskList, Filter, State),
    updateColumns(Filter2, State),
    ETodos = eTodoDB:getETodos(User, TaskList3, Filter2, SearchText, Cfg),

    wx:batch(fun() ->
                     clearAndInitiate(TodoList, length(ETodos)),
                     lists:foldl(fun (ETodo, Acc) ->
                                         setRowInfo(TodoList, ETodo, Acc),
                                         Acc + 1
                                 end, 0, ETodos)
             end),

    Count  = wxListCtrl:getItemCount(TodoList),
    State3 = case getActive(State) of
                 Active when (Active >= 0) and (Active < Count) ->
                     wxListCtrl:setItemState(TodoList, getActive(State),
                                             ?wxLIST_STATE_SELECTED,
                                             ?wxLIST_STATE_SELECTED),
                     State;
                 Active when Active == -1 ->
                     State;
                 _ ->
                     case Count > 0 of
                         true ->
                             ETodo  = getETodoAtIndex(0, State#guiState.rows),
                             State2 = updateGui(ETodo, 0, State),
                             State2#guiState{activeTodo = {ETodo, 0}};
                         false ->
                             State2 = updateGui(undefined, 0, State),
                             State2#guiState{activeTodo = {undefined, -1}}
                     end
             end,
    updateInfoIcon(State3),
    State3#guiState{rows = eRows:replace(ETodos, State#guiState.rows)}.


getActive(#guiState{activeTodo = {_, Index}}) when Index > 0 -> Index;
getActive(_)                                                 -> -1.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
wxDate2Date({{1970, _, _}, {_, _, _}}) -> undefined;
wxDate2Date({{YY, MM, DD}, {_, _, _}}) -> {YY, MM, DD};
wxDate2Date({YY, MM, DD})              -> {YY, MM, DD};
wxDate2Date(undefined)                 -> undefined.

date2wxDate({YY, MM, DD})              -> {{YY, MM, DD}, {0, 0, 0}};
date2wxDate({{YY, MM, DD}, {_, _, _}}) -> {{YY, MM, DD}, {0, 0, 0}};
date2wxDate(undefined)                 -> {date(), {0, 0, 0}}.

%%======================================================================
%% Help functions for menu creation.
%%======================================================================
showBookmarkMenu(Items, State = #guiState{popupBookmMenu = OldMenu,
                                          frame          = Frame}) ->
    removeOldMenu(OldMenu),
    Menu = wxMenu:new(),
    makeMenuItems(Menu, ?bookmarks + 1, Items),
    wxMenu:connect(Menu, command_menu_selected),
    wxWindow:popupMenu(Frame, Menu),
    State#guiState{popupBookmMenu = Menu}.

makeMenuItems(_Menu, _BookmarkItem, []) ->
    ok;
makeMenuItems(Menu, BookmarkItem, [Bookmark | Rest]) ->
    wxMenu:append(Menu, BookmarkItem, Bookmark),
    makeMenuItems(Menu, BookmarkItem + 1, Rest).

showMenu(_User, {row, -1}, Frame, State = #guiState{popUpMenu = OldMenu}) ->
    removeOldMenu(OldMenu),
    Menu = wxMenu:new(),
    createPluginMenu(Menu, undefined),
    wxMenu:connect(Menu, command_menu_selected),
    wxWindow:popupMenu(Frame, Menu),
    State#guiState{popUpMenu = Menu, popUpCol  = {row, -1}};
showMenu(User, {row, Row}, Frame, State = #guiState{popUpMenu = OldMenu}) ->
    removeOldMenu(OldMenu),
    Menu = createMenu(User, Row, State),
    wxMenu:connect(Menu, command_menu_selected),
    wxWindow:popupMenu(Frame, Menu),
    State#guiState{popUpMenu = Menu, popUpCol  = {row, Row}};
showMenu(User, Column, Frame, State = #guiState{popUpMenu = OldMenu,
                                                filter    = Filter}) ->
    removeOldMenu(OldMenu),
    Menu = createMenu(User, Column, Filter),
    wxMenu:connect(Menu, command_menu_selected),
    wxWindow:popupMenu(Frame, Menu),
    State#guiState{popUpMenu = Menu, popUpCol  = Column}.

showMenu(Uid, State = #guiState{frame = Frame, popUpMenu = OldMenu}) ->
    removeOldMenu(OldMenu),
    Menu = createMenu(Uid, State),
    wxMenu:connect(Menu, command_menu_selected),
    wxWindow:popupMenu(Frame, Menu),
    State#guiState{popUpMenu = Menu, popUpCol = {row, -1}}.

removeOldMenu(undefined) -> ok;
removeOldMenu(Menu)      -> wxMenu:destroy(Menu).

createPluginMenu(Menu, ETodo) ->
    MenuOpts = ePluginServer:getRightMenuForPlugins(ETodo),
    case MenuOpts of
        [] ->
            ok;
        MenuOpts ->
            createPluginMenu2(Menu, MenuOpts)
    end.

createPluginMenu2(_Menu, []) ->
    ok;
createPluginMenu2(Menu, [divider|Rest]) ->
    wxMenu:appendSeparator(Menu),
    createPluginMenu2(Menu, Rest);
createPluginMenu2(Menu, [{{MenuType, Name}, MenuOptions}|Rest])
  when (MenuType == pluginName) or (MenuType == subMenu) ->
    SubMenu     = wxMenu:new([]),
    SubMenuItem = wxMenuItem:new([{parentMenu, Menu}, {id, ?wxID_ANY},
                                  {text, Name}, {subMenu, SubMenu},
                                  {kind, ?wxITEM_NORMAL}]),
    wxMenu:append(Menu, SubMenuItem),
    createPluginMenu2(SubMenu, MenuOptions),
    wxMenu:connect(SubMenu, command_menu_selected),
    createPluginMenu2(Menu, Rest);
createPluginMenu2(SubMenu, [{MenuOption, MenuText}|Rest]) ->
    wxMenu:append(SubMenu, MenuOption, MenuText),
    createPluginMenu2(SubMenu, Rest).

createMenu(Uid, #guiState{user = User, columns = Columns}) ->
    Menu        = wxMenu:new(),
    %% Create Plugin menu options

    Todo  = eTodoDB:getTodo(Uid),
    ETodo = eTodoUtils:makeETodo(Todo, User, Columns),
    createPluginMenu(Menu, ETodo),

    Menu.

createMenu(User, Row, State = #guiState{}) ->
    Menu        = wxMenu:new(),
    %% Create Status submenu
    SubMenu     = wxMenu:new([]),
    SubMenuItem = wxMenuItem:new([{parentMenu, Menu}, {id, ?wxID_ANY},
                                  {text, "Status"}, {subMenu, SubMenu},
                                  {kind, ?wxITEM_NORMAL}]),
    wxMenu:append(Menu, SubMenuItem),
    ID1  = wxMenu:appendRadioItem(SubMenu, ?statusPlanning,   ?descPlanning),
    ID2  = wxMenu:appendRadioItem(SubMenu, ?statusInProgress, ?descInProgress),
    ID3  = wxMenu:appendRadioItem(SubMenu, ?statusDone,       ?descDone),
    ID4  = wxMenu:appendRadioItem(SubMenu, ?statusNone,       ?descNA),

    wxMenu:connect(SubMenu, command_menu_selected),
    setDefault(User, Row, Menu, ID1, ID2, ID3, ID4, State),

    SubMenu2     = wxMenu:new([]),
    SubMenuItem2 = wxMenuItem:new([{parentMenu, Menu}, {id, ?wxID_ANY},
                                   {text, "Lists"}, {subMenu, SubMenu2},
                                   {kind, ?wxITEM_NORMAL}]),
    wxMenu:append(Menu, SubMenuItem2),

    %% Create Lists submenu
    TodoLists = getTodoLists(User),
    Rows      = State#guiState.rows,
    ETodo     = getETodoAtIndex(Row, Rows),
    createSubMenu(SubMenu2, ?lists + 1, ETodo, TodoLists),

    wxMenu:connect(SubMenu2, command_menu_selected),

    %% Create Plugin menu options
    createPluginMenu(Menu, ETodo),

    Menu;
createMenu(User, Column, Filter) ->
    Menu = wxMenu:new(),
    Def  = wxMenu:appendRadioItem(Menu, ?sortDef, ?descSortDef),
    Asc  = wxMenu:appendRadioItem(Menu, ?sortAsc, ?descSortAsc),
    Dec  = wxMenu:appendRadioItem(Menu, ?sortDec, ?descSortDec),
    setDefault(User, Column, Menu, Def, Asc, Dec),
    addFilter(Menu, Column, Filter),

    %% Add column show configuration
    Columns = [Desc || {_Col, Desc} <- eTodoDB:getColumns(User)],
    SubMenu = wxMenu:new([]),
    SubMenuItem = wxMenuItem:new([{parentMenu, Menu}, {id, ?wxID_ANY},
                                  {text, "Show columns"}, {subMenu, SubMenu},
                                  {kind, ?wxITEM_NORMAL}]),
    createSubMenuForColumns(User, SubMenu, ?showcolumns + 1, Columns),
    wxMenu:append(Menu, SubMenuItem),
    wxMenu:append(Menu, ?sortColumns, "Order columns"),
    wxMenu:connect(SubMenu, command_menu_selected),
    Menu.

createSubMenuForColumns(_User, _SubMenu, _ColItem, []) ->
    ok;
createSubMenuForColumns(User, SubMenu, ColItem, [Col | Rest]) ->
    ID    = wxMenu:appendCheckItem(SubMenu, ColItem, Col),
    Check = checkVisable(User, Col),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID), Check),
    createSubMenuForColumns(User, SubMenu, ColItem + 1, Rest).

checkVisable(User, Col) ->
    ColInternal = eTodoUtils:taskInternal(Col),
    default(eTodoDB:readListCfg(User, ColInternal, visible), true).

createSubMenu(_SubMenu, _ListItem, _ETodo, []) ->
    ok;
createSubMenu(SubMenu, ListItem, ETodo, [List | Rest]) ->
    ID    = wxMenu:appendCheckItem(SubMenu, ListItem, List),
    Check = lists:member(List, ETodo#etodo.listsDB),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID), Check),
    createSubMenu(SubMenu, ListItem + 1, ETodo, Rest).

setDefault(_User, Row, Menu, ID1, ID2, ID3, ID4, State) ->
    ETodo = getETodoAtIndex(Row, State#guiState.rows),
    case ETodo#etodo.statusDB of
        planning   -> wxMenu:check(Menu, wxMenuItem:getId(ID1), true);
        inProgress -> wxMenu:check(Menu, wxMenuItem:getId(ID2), true);
        done       -> wxMenu:check(Menu, wxMenuItem:getId(ID3), true);
        undefined  -> wxMenu:check(Menu, wxMenuItem:getId(ID4), true)
    end.

setDefault(User, Col, Menu, Def, Asc, Dec) ->
    case default(eTodoDB:readListCfg(User, sorted), default) of
        {ascending,  Col} -> wxMenu:check(Menu, wxMenuItem:getId(Asc), true);
        {descending, Col} -> wxMenu:check(Menu, wxMenuItem:getId(Dec), true);
        default           -> wxMenu:check(Menu, wxMenuItem:getId(Def), true);
        {_, Column}       ->
            Text  = "Sorted on column \"" ++ Column ++ "\"",
            MItem = wxMenu:appendRadioItem(Menu, ?wxID_ANY, Text),
            wxMenu:check(Menu, wxMenuItem:getId(MItem), true)
    end.

addFilter(Menu, Column, Filter) when (Column == ?status) or
                                     (Column == ?prio)   or
                                     (Column == ?uid)    ->
    wxMenu:appendSeparator(Menu),
    SubMenu = wxMenu:new([]),
    appendValues(SubMenu, Column, Filter),
    SubMenuItem = wxMenuItem:new([{parentMenu, Menu}, {id, ?wxID_ANY},
                                  {text, "Filter"}, {subMenu, SubMenu},
                                  {kind, ?wxITEM_NORMAL}]),
    wxMenu:append(Menu, SubMenuItem),
    wxMenu:connect(SubMenu, command_menu_selected);
addFilter(_, _, _) ->
    ok.

appendValues(SubMenu, ?status, Val) ->
    ID1 = wxMenu:appendCheckItem(SubMenu, ?statusPlanning,   ?descPlanning),
    ID2 = wxMenu:appendCheckItem(SubMenu, ?statusInProgress, ?descInProgress),
    ID3 = wxMenu:appendCheckItem(SubMenu, ?statusDone,       ?descDone),
    ID4 = wxMenu:appendCheckItem(SubMenu, ?statusNone,       ?descNA),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID1), check(?statusPlanning,   Val)),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID2), check(?statusInProgress, Val)),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID3), check(?statusDone,       Val)),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID4), check(?statusNone,       Val));
appendValues(SubMenu, ?prio, Val) ->
    ID1 = wxMenu:appendCheckItem(SubMenu, ?prioLow,    ?descLow),
    ID2 = wxMenu:appendCheckItem(SubMenu, ?prioMedium, ?descMedium),
    ID3 = wxMenu:appendCheckItem(SubMenu, ?prioHigh,   ?descHigh),
    ID4 = wxMenu:appendCheckItem(SubMenu, ?prioNone,   ?descNA),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID1), check(?prioLow,    Val)),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID2), check(?prioMedium, Val)),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID3), check(?prioHigh,   Val)),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID4), check(?prioNone,   Val));
appendValues(SubMenu, ?uid, Val) ->
    ID1 = wxMenu:appendCheckItem(SubMenu, ?assigned, ?descAssigned),
    wxMenu:check(SubMenu, wxMenuItem:getId(ID1), check(?assigned, Val)).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
clearAndInitiate(TodoList, Rows) ->
    wxListCtrl:deleteAllItems(TodoList),
    insertRows(TodoList, Rows).

insertRows(_TodoList, 0)   -> ok;
insertRows(TodoList,  Row) ->
    wxListCtrl:insertItem(TodoList, Row, ""),
    insertRows(TodoList, Row - 1).


check(Key, Val) -> not lists:member(Key, Val).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
checkStatus(State = #guiState{user       = User,
                              activeTodo = ActiveTodo,
                              clipBoard  = ClipboardContent,
                              loggedIn   = LoggedIn}) ->
    TDown    = xrcId("todoDownTool"),
    MUp      = xrcId("moveUpTool"),
    MFiM     = xrcId("moveFirstMenu"),
    MUpM     = xrcId("moveUpMenu"),
    MDown    = xrcId("moveDownTool"),
    MDownM   = xrcId("moveDownMenu"),
    MLaM     = xrcId("moveLastMenu"),
    Cut      = xrcId("cutTool"),
    Copy     = xrcId("copyTool"),
    Paste    = xrcId("pasteTool"),
    Delete   = xrcId("deleteTool"),
    CutM     = xrcId("cutMenu"),
    CopyM    = xrcId("copyMenu"),
    PasteM   = xrcId("pasteMenu"),
    DeleteM  = xrcId("deleteMenu"),
    Forward  = xrcId("forwardTool"),
    Backup   = xrcId("backupMenuItem"),
    Restore  = xrcId("restoreMenuItem"),

    Reminder = obj("setReminderButton",  State),
    Shared   = obj("shareButton",        State),
    TaskEdit = obj("taskEditPanel",      State),
    DescArea = obj("descriptionAreaSTC", State),
    ComArea  = obj("commentAreaSTC",     State),

    ToolBar   = State#guiState.toolBar,
    MenuBar   = State#guiState.menuBar,
    TaskList  = getTaskList(State),
    TodoList  = getTodoList(TaskList, State),
    Selected  = findSelected(TodoList) =/= -1,
    Clipboard = (ClipboardContent =/= undefined),
    AllTask   = TaskList == ?defTaskList,

    wxBitmapButton:enable(Reminder, [{enable, Selected}]),
    wxBitmapButton:enable(Shared, [{enable, Selected}]),

    NoSubTodos = case ActiveTodo of
                     {ETodo = #etodo{}, _} ->
                         not ETodo#etodo.hasSubTodo;
                     _ ->
                         false
                 end,
    case default(eTodoDB:readListCfg(User, sorted), default) of
        default ->
            wxToolBar:enableTool(ToolBar, TDown,   true),
            wxMenuBar:enable(MenuBar,     MUpM,    Selected),
            wxMenuBar:enable(MenuBar,     MFiM,    Selected),
            wxMenuBar:enable(MenuBar,     MDownM,  Selected),
            wxMenuBar:enable(MenuBar,     MLaM,    Selected),
            wxToolBar:enableTool(ToolBar, MDown,   Selected),
            wxToolBar:enableTool(ToolBar, MUp,     Selected);
        _ ->
            wxMenuBar:enable(MenuBar,     MFiM,    false),
            wxMenuBar:enable(MenuBar,     MUpM,    false),
            wxMenuBar:enable(MenuBar,     MDownM,  false),
            wxMenuBar:enable(MenuBar,     MLaM,    false),
            wxToolBar:enableTool(ToolBar, MDown,   false),
            wxToolBar:enableTool(ToolBar, MUp,     false),
            wxToolBar:enableTool(ToolBar, TDown,   false)
    end,

    wxToolBar:enableTool(ToolBar, Cut,
                         Selected and NoSubTodos and not AllTask),
    wxToolBar:enableTool(ToolBar, Paste,   Clipboard),
    wxToolBar:enableTool(ToolBar, Copy,    Selected),
    wxToolBar:enableTool(ToolBar, Delete,  Selected and NoSubTodos),
    wxToolBar:enableTool(ToolBar, Forward, Selected and (not NoSubTodos)),
    wxMenuBar:enable(MenuBar,     CutM,
                     Selected and NoSubTodos and not AllTask),
    wxMenuBar:enable(MenuBar,     PasteM,  Clipboard),
    wxMenuBar:enable(MenuBar,     CopyM,   Selected),
    wxMenuBar:enable(MenuBar,     DeleteM, Selected and NoSubTodos),
    wxMenuBar:enable(MenuBar,     Backup,  not LoggedIn),
    wxMenuBar:enable(MenuBar,     Restore, not LoggedIn),

    wxPanel:enable(TaskEdit, [{enable, Selected}]),

    wxStyledTextCtrl:setReadOnly(DescArea, not Selected),
    wxStyledTextCtrl:setReadOnly(ComArea, not Selected),

    checkUndoStatus(State).


checkUndoStatus(State) ->
    UndoM    = xrcId("undoMenu"),
    RedoM    = xrcId("redoMenu"),
    Undo     = xrcId("undoTool"),
    Redo     = xrcId("redoTool"),

    ToolBar  = State#guiState.toolBar,
    MenuBar  = State#guiState.menuBar,

    {UndoStatus, RedoStatus} = eTodoDB:undoStatus(),

    wxToolBar:enableTool(ToolBar, Undo,    UndoStatus),
    wxToolBar:enableTool(ToolBar, Redo,    RedoStatus),
    wxMenuBar:enable(MenuBar,     UndoM,   UndoStatus),
    wxMenuBar:enable(MenuBar,     RedoM,   RedoStatus),
    State.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
userStatusUpdate(State = #guiState{userStatus = UserList,
                                   user       = User}) ->
    Obj          = obj("userStatusChoice", State),
    Obj2         = obj("userStatusMsg",    State),
    Index        = wxChoice:getSelection(Obj),
    Status       = wxChoice:getString(Obj, Index),
    case Status of
        "Offline" ->
            doLogout(User, State);
        _->
            setUserStatus(Status, State),
            (catch setPortrait(User, State)),
            deselectUserCheckBox(State),
            StatusMsg    = wxComboBox:getValue(Obj2),
            StatusUpdate = #userStatus{userName  = User,
                                       statusMsg = StatusMsg,
                                       status    = Status},
            ePluginServer:eSetStatusUpdate(User, Status, StatusMsg),
            eTodoStatus:set_status(User, Status, StatusMsg),
            Users = [UserStatus#userStatus.userName || UserStatus <- UserList],
            ePeerEM:sendMsg(User, Users, statusEntry,
                            {statusUpdate, StatusUpdate, getPortrait(User)}),
            MsgTop = obj("msgTopPanel", State),
            wxPanel:layout(MsgTop),
            wxPanel:refresh(MsgTop),
            State
    end.

deselectUserCheckBox(State) ->
    %% Uncheck selection in userCheckBox.
    Obj   = obj("userCheckBox", State),
    Index = wxCheckListBox:getSelection(Obj),
    case Index of
        Index when Index >= 0 ->
            wxCheckListBox:deselect(Obj, Index);
        _ ->
            ok
    end.

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

    ShareText = makeStr(default(SharedWith, [User])),
    Lists     = eTodoDB:getLists(User, Uid),

    #etodo{status         = toStr(Status),
           statusCol      = col(?status, Columns),
           statusDB       = Status,
           priority       = toStr(Priority),
           priorityCol    = col(?prio, Columns),
           priorityDB     = Priority,
           owner          = toStr(default(Owner, User)),
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
           hasSubTodo     = eTodoDB:hasSubTodo(Uid),
           uid            = toStr(Uid),
           uidCol         = col(?uid, Columns),
           uidDB          = Uid,
           progress       = Progress,
           lists          = makeStr(Lists),
           listsDB        = Lists}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
xrcId(Id) ->
    %% Cache for faster access.
    case get({wxXrcId, Id}) of
        undefined ->
            Ret = wxXmlResource:getXRCID(Id),
            put({wxXrcId, Id}, Ret),
            Ret;
        Value ->
            Value
    end.

%%======================================================================
%% Function : sendUpdate(User, Owner, ETodo) -> ok
%% Purpose  : Send message that owner has been updated to owner.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
sendUpdate(User, User, _ETodo) ->
    ok;
sendUpdate(User, Owner, ETodo) ->
    Users = ETodo#etodo.sharedWithDB,
    case lists:member(Owner, Users) of
        true ->
            Text = "You are the new owner of: " ++ ETodo#etodo.description,
            eTodo:systemEntry(ETodo#etodo.uidDB, Text),
            ePeerEM:sendMsg(User, [Owner],
                            {systemEntry, ETodo#etodo.uidDB}, Text);
        false ->
            %% The task isn't shared with owner.
            eLog:log(debug, ?MODULE, sendUpdate, [Owner, Users],
                     "Task not shared with owner, do not send update.", ?LINE),
            ok
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
clearStatusBar(State) ->
    StatusBarObj = obj("mainStatusBar", State),
    wxStatusBar:setStatusText(StatusBarObj, "", [{number, 2}]),
    State.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateInfoIcon(State = #guiState{filter = Filter})
  when (Filter == undefined) or (Filter == []) ->
    InfoIcon = obj("infoIcon", State),
    Panel    = obj("mainPanel", State),
    UseFlt   = obj("checkBoxUseFilter", State),
    wxStaticBitmap:setToolTip(InfoIcon, ""),
    wxStaticBitmap:hide(InfoIcon),
    wxCheckBox:disable(UseFlt),
    wxPanel:layout(Panel);
updateInfoIcon(State) ->
    InfoIcon = obj("infoIcon", State),
    Panel    = obj("mainPanel", State),
    UseFlt   = obj("checkBoxUseFilter", State),
    Text     = "Filter is used for all columns marked with *",
    wxStaticBitmap:setToolTip(InfoIcon, Text),
    wxStaticBitmap:show(InfoIcon),
    wxCheckBox:enable(UseFlt),
    wxPanel:layout(Panel).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateColumns(_Filter, State = #guiState{columns = undefined}) ->
    State;
updateColumns(Filter, State = #guiState{columns = Columns}) ->
    FilterCols = [eTodoUtils:toColumn(Flt) || Flt <- Filter],
    updateColumns(FilterCols, Columns, State).

updateColumns(_FilterCols, [], State) ->
    State;
updateColumns(FilterCols, [{ColNr, Column}|Rest], State) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    NewCol   = wxListItem:new(),
    wxListItem:setMask(NewCol, ?wxLIST_MASK_TEXT),
    case lists:member(Column, FilterCols) of
        true ->
            wxListItem:setText(NewCol, Column ++ "*");
        false ->
            wxListItem:setText(NewCol, Column)
    end,
    wxListCtrl:setColumn(TodoList, ColNr, NewCol),
    updateColumns(FilterCols, Rest, State).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
useFilter(_TaskList, Filter, #guiState{mode = noGui}) ->
    Filter;
useFilter(TaskList, Filter, State) ->
    UseFlt = obj("checkBoxUseFilter", State),
    case wxCheckBox:isChecked(UseFlt) of
        true ->
            case lists:keyfind(TaskList, 1, Filter) of
                {TaskList, Flt} ->
                    Flt;
                _ ->
                    %% Get default value for all lists
                    lists:filter(fun(X) -> not is_tuple(X) end, Filter)
            end;
        false ->
            []
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
findSelected(TodoList) ->
    wxListCtrl:getNextItem(TodoList, -1, [{state, ?wxLIST_STATE_SELECTED}]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
pos(Value, List) -> pos(Value, List, []).

pos(Value, [Value | _List], Pos) -> length(Pos);
pos(Value, [_ | List], SoFar)    -> pos(Value, List, [Value | SoFar]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
userStatusAvailable(State) ->
    Obj1 = obj("userAvailableIcon", State),
    Obj2 = obj("userBusyIcon",      State),
    Obj3 = obj("userAwayIcon",      State),
    Obj4 = obj("userOfflineIcon",   State),
    Obj5 = obj("userStatusChoice",  State),
    Obj6 = obj("allMsgPanel",       State),

    %% Set correct icon to show
    wxStaticBitmap:show(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:hide(Obj4),

    %% Enable and set combobox to available.
    wxChoice:enable(Obj5),
    setSelection(Obj5, "Available"),

    wxPanel:layout(Obj6),
    wxPanel:refresh(Obj6),
    State.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
userStatusOffline(State) ->
    Obj1 = obj("userAvailableIcon", State),
    Obj2 = obj("userBusyIcon",      State),
    Obj3 = obj("userAwayIcon",      State),
    Obj4 = obj("userOfflineIcon",   State),
    Obj5 = obj("userStatusChoice",  State),
    Obj6 = obj("allMsgPanel",       State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:show(Obj4),

    %% Disable and set combo box to offline.
    setSelection(Obj5, "Offline"),
    wxChoice:disable(Obj5),

    wxPanel:layout(Obj6),
    wxPanel:refresh(Obj6),
    State.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
userStatusBusy(State) ->
    Obj1 = obj("userAvailableIcon", State),
    Obj2 = obj("userBusyIcon",      State),
    Obj3 = obj("userAwayIcon",      State),
    Obj4 = obj("userOfflineIcon",   State),
    Obj5 = obj("allMsgPanel",       State),
    Obj6 = obj("userStatusChoice",  State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj2),

    %% Enable and set combobox to busy.
    wxChoice:enable(Obj6),
    setSelection(Obj6, "Busy"),

    wxPanel:layout(Obj5),
    wxPanel:refresh(Obj5),
    State.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
userStatusAway(State) ->
    Obj1 = obj("userAvailableIcon", State),
    Obj2 = obj("userBusyIcon",      State),
    Obj3 = obj("userAwayIcon",      State),
    Obj4 = obj("userOfflineIcon",   State),
    Obj5 = obj("allMsgPanel",       State),
    Obj6 = obj("userStatusChoice",  State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj3),

    %% Enable and set combobox to available.
    wxChoice:enable(Obj6),
    setSelection(Obj6, "Away"),

    wxPanel:layout(Obj5),
    wxPanel:refresh(Obj5),
    State.

setUserStatus("Away",      State) -> userStatusAway(State);
setUserStatus("Busy",      State) -> userStatusBusy(State);
setUserStatus("Offline",   State) -> userStatusOffline(State);
setUserStatus("Available", State) -> userStatusAvailable(State);
setUserStatus(_Unknown,    State) -> State.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
setPeerStatus(UserStatus = #userStatus{status = "Available"}, State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),
    Obj4 = obj("peerOfflineIcon",   State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj1),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(UserStatus = #userStatus{status = "Busy"}, State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),
    Obj4 = obj("peerOfflineIcon",   State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj2),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(UserStatus = #userStatus{status = "Away"}, State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),
    Obj4 = obj("peerOfflineIcon",   State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj3),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(UserStatus = #userStatus{status = "Offline"}, State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),
    Obj4 = obj("peerOfflineIcon",   State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:show(Obj4),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(UserStatus = #userStatus{}, State) ->
    setPeerStatus(UserStatus#userStatus{status    = "Away",
                                        statusMsg = ""}, State);
setPeerStatus(Peer, State = #guiState{user = User}) ->
    case eTodoDB:getConnection(Peer) of
        #conCfg{email = EmailAddress} ->
            setPeerStatus(#userStatus{userName  = Peer,
                                      statusMsg = EmailAddress,
                                      status    = "Offline"}, State);
        _ ->
            case getPeerEmail(User, Peer) of
                undefined ->
                    State;
                EmailAddress ->
                    setPeerStatus(#userStatus{userName  = Peer,
                                              statusMsg = EmailAddress,
                                              status    = "Offline"}, State)
            end
    end.

getPeerEmail(User, Peer) ->
    UserCfg       = eTodoDB:readUserCfg(User),
    ExternalUsers = default(UserCfg#userCfg.ownerCfg, []),
    doGetPeerEmail(Peer, ExternalUsers).

doGetPeerEmail(_Peer, []) ->
    undefined;
doGetPeerEmail(Peer, [PeerInfo|Rest]) ->
    case eTodoUtils:getPeerInfo(PeerInfo) of
        {Peer, Email} ->
            Email;
        _ ->
            doGetPeerEmail(Peer, Rest)
    end.

setPeerStatusTextAndLayout(#userStatus{userName  = UserName,
                                       statusMsg = Message}, State) ->
    Obj1 = obj("userStatusPanel", State),
    wxPanel:layout(Obj1),
    wxPanel:refresh(Obj1),

    PeerMessage =
        case Message of
            "" ->
                UserName;
            _ ->
                UserName ++ ": " ++ Message
        end,
    Obj2 = obj("peerStatusMsg", State),
    wxStaticText:setLabel(Obj2,   PeerMessage),
    wxStaticText:setToolTip(Obj2, PeerMessage),

    (catch setPortrait(UserName, State)),
    State.

setPortrait(Peer, State) ->
    CustomPortrait = getRootDir() ++ "/Icons/portrait_" ++ Peer ++ ".png",
    FileName = case filelib:is_file(CustomPortrait) of
                   true ->
                       getRootDir() ++ "/Icons/portrait_" ++ Peer ++ ".png";
                   false ->
                       case Peer of
                           "Slack - " ++ SlackUser ->
                               setPortrait(SlackUser, State);
                           _ ->
                               getRootDir() ++ "/Icons/portrait.png"
                       end
               end,
    Png = wxImage:new(FileName),
    Bitmap = wxBitmap:new(Png),
    wxBitmap:setHeight(Bitmap, 64),
    wxBitmap:setWidth(Bitmap, 64),
    case (State#guiState.user == Peer) of
        true ->
            wxStaticBitmap:setBitmap(obj("portraitUserIcon", State), Bitmap);
        false ->
            wxStaticBitmap:setBitmap(obj("portraitPeerIcon", State), Bitmap)
    end.

getPortrait(Peer) ->
    CustomPortrait = getRootDir() ++ "/Icons/portrait_" ++ Peer ++ ".png",
    case filelib:is_file(CustomPortrait) of
        true ->
            {ok, Bin} = file:read_file(CustomPortrait),
            Bin;
        false ->
            undefined
    end.

setPeerStatusIfNeeded(State = #guiState{userStatus = Users}) ->
    Obj     = obj("userCheckBox", State),
    Index   = wxCheckListBox:getSelection(Obj),
    Focus   = wxWindow:findFocus(),
    UserCB  = equal(Focus, Obj),
    case {Index >= 0, UserCB} of
        {true, true} ->
            User  = wxCheckListBox:getString(Obj, Index),
            case lists:keyfind(User, #userStatus.userName, Users) of
                UserStatus when is_record(UserStatus, userStatus) ->
                    setPeerStatus(UserStatus, State);
                false ->
                    clearPeerStatus(State)
            end;
        {false, true} ->
            State;
        _ ->
            Obj2    = obj("emailCheckBox", State),
            Index2  = wxCheckListBox:getSelection(Obj2),
            EmailCB = equal(Focus, Obj2),
            case {Index2 >= 0, EmailCB} of
                {true, true} ->
                    OfflineUser = wxCheckListBox:getString(Obj2, Index2),
                    setPeerStatus(OfflineUser, State);
                _ ->
                    State
            end
    end,
    State.

equal(Obj1, Obj2) ->
    case wx:typeCast(Obj1, wxCheckListBox) of
        Obj2 ->
            true;
        _ ->
            false
    end.

clearPeerStatus(State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj3),

    Obj4 = obj("userStatusPanel", State),
    wxPanel:layout(Obj4),
    wxPanel:refresh(Obj4),

    Obj5 = obj("peerStatusMsg", State),
    PeerMessage = "Select a peer to show status information here...",
    wxStaticText:setLabel(Obj5,   PeerMessage),
    wxStaticText:setToolTip(Obj5, PeerMessage),
    State.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

saveColumnSizes(State = #guiState{columns = Columns,
                                  user    = User}) ->
    TaskList  = getTaskList(State),
    TodoList  = getTodoList(TaskList, State),
    [saveColumnSizes(TodoList, User, Col, Desc) || {Col, Desc} <- Columns].

saveColumnSizes(TodoList, User, Col, Desc) ->
    InternalName = eTodoUtils:taskInternal(Desc),
    ColumnSize   = wxListCtrl:getColumnWidth(TodoList, Col),
    case eTodoDB:readListCfg(User, InternalName, visible) of
        false ->
            ok;
        _ ->
            eTodoDB:saveListCfg(User, InternalName, columnWidth, ColumnSize)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

setColumnWidth(TodoList, User, Col, ?description) ->
    setColumnWidth(TodoList, User, Col, ?description, 200);
setColumnWidth(TodoList, User, Col, ?status) ->
    setColumnWidth(TodoList, User, Col, ?status, 110);
setColumnWidth(TodoList, User, Col, ?comment) ->
    setColumnWidth(TodoList, User, Col, ?comment, 100);
setColumnWidth(TodoList, User, Col, Field) ->
    setColumnWidth(TodoList, User, Col, Field, ?wxLIST_AUTOSIZE_USEHEADER).

setColumnWidth(TodoList, User, Col, Desc, Default) ->
    TaskInternal = eTodoUtils:taskInternal(Desc),
    case eTodoDB:readListCfg(User, TaskInternal, visible) of
        false ->
            wxListCtrl:setColumnWidth(TodoList, Col, 0);
        _ ->
            case eTodoDB:readListCfg(User, TaskInternal, columnWidth) of
                Num when is_integer(Num) and (Num > 0) ->
                    wxListCtrl:setColumnWidth(TodoList, Col, Num);
                _ ->
                    wxListCtrl:setColumnWidth(TodoList, Col, Default)
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
generateWorkLog(State = #guiState{user = User}) ->
    Obj1      = obj("workLogStartDate", State),
    Obj2      = obj("workLogReport",    State),
    DateTime  = wxDatePickerCtrl:getValue(Obj1),
    {Date, _} = DateTime,
    ePluginServer:eSetWorkLogDate(User, Date),

    eWorkLogView:load(Obj2, User, Date),
    State.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
generateTimeLog(State = #guiState{user = User}) ->
    Obj      = obj("timeLogReport", State),
    TaskList = getTaskList(State),
    eWorkLogView:load(Obj, User, "time-log", TaskList),
    State.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
generateSchedule(State = #guiState{user = User}) ->
    Obj    = obj("scheduleReport", State),
    eWorkLogView:load(Obj, User, "schedule", ?defTaskList),
    State.

%% Hidden report tabs are regenerated when they are selected.
refreshVisibleReport(State = #guiState{loggedIn = false}) ->
    State;
refreshVisibleReport(State) ->
    Notebook = obj("mainNotebook", State),
    Current  = wxNotebook:getCurrentPage(Notebook),
    Reports  = [{2, fun generateWorkLog/1},
                {3, fun generateTimeLog/1},
                {4, fun generateSchedule/1}],
    case [Generate || {Index, Generate} <- Reports,
                      wxNotebook:getPage(Notebook, Index) =:= Current] of
        [Generate] -> Generate(State);
        []         -> State
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
getWorkDesc("",    Description) -> smartSplit(Description);
getWorkDesc(Desc, _Description) -> Desc.

smartSplit(Text) ->
    smartSplit(Text, []).

smartSplit([], SoFar) ->
    lists:reverse(SoFar);
smartSplit([Char|_], SoFar) when (Char == 10) or (Char == 13) ->
    lists:reverse(SoFar);
smartSplit([Char|_], SoFar) when (Char == 32) and (length(SoFar) > 35) ->
    lists:reverse(SoFar);
smartSplit(_Text, SoFar) when length(SoFar) > 45 ->
    string:sub_string(lists:reverse(SoFar), 1, 42) ++ "...";
smartSplit([Char|Rest], SoFar) ->
    smartSplit(Rest, [Char|SoFar]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
saveMsg(_UserName, State) ->
    saveMsgCounters(State).

%% Total and per-kind counters are stored together so a restart restores both.
saveMsgCounters(State = #guiState{user = User, unreadMsgs2 = {C1, C2, C3}}) ->
    UserCfg = eTodoDB:readUserCfg(User),
    eTodoDB:saveUserCfg(UserCfg#userCfg{unreadMsgs  = C1 + C2 + C3,
                                        unreadMsgs2 = {C1, C2, C3}}),
    eTodoPush:notify(User, <<"counts">>),
    State.

showMsgCounters(State = #guiState{unreadMsgs2 = {C1, C2, C3}}) ->
    setMsgCounter(obj("mainNotebook", State), C1 + C2 + C3),
    State#guiState{unreadMsgs = C1 + C2 + C3}.

%% Messages were read elsewhere, e.g. in the web GUI.
markMessagesRead(State) ->
    State2 = clearNotificationTimer(State#guiState{unreadMsgs2 = {0, 0, 0}}),
    saveMsgCounters(showMsgCounters(State2)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
deleteAndUpdate(Index, TodoList, State) ->
    Size = wxListCtrl:getItemCount(TodoList),
    deleteAndUpdate(Size, Index, TodoList, State).

deleteAndUpdate(Size, Index, _TodoList, State) when Size =< Index ->
    State;
deleteAndUpdate(Size, Index, TodoList, State) ->
    setColor(TodoList, Index),
    deleteAndUpdate(Size, Index + 1, TodoList, State).

%%======================================================================
%% Function : toClipboard(Text, State) -> ok
%% Purpose  : Copy text to clipboard.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
toClipboard(Text) ->
    toClipboard(Text, ok).

toClipboard(Text, State) ->
    ClipBoard = wxClipboard:get(),
    case wxClipboard:open(ClipBoard) of
        true ->
            TextObj = wxTextDataObject:new([{text, Text}]),
            wxClipboard:setData(ClipBoard, TextObj),
            wxClipboard:close(ClipBoard),
            State;
        false ->
            State
    end.

%%======================================================================
%% Function : fillEmailCheckBox(State) -> void
%% Purpose  : Fill email check box with all email users.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
fillEmailCheckBox(State = #guiState{user = User}) ->
    EmailCheckBox = obj("emailCheckBox", State),
    Filter = fun (#conCfg{email    = undefined}) -> false;
                 (#conCfg{email    = ""})        -> false;
                 (#conCfg{})                     -> true
             end,
    ConWithEmail = lists:filter(Filter, eTodoDB:getConnections()),
    EmailUsers   = [EmailUser || #conCfg{userName = EmailUser} <- ConWithEmail],
    OffLineUsers = lists:filter(fun(EmailUser) ->
                                    not ePeerCircle:isActivePeer(EmailUser)
                                end, EmailUsers),
    wxCheckListBox:clear(EmailCheckBox),

    UserCfg   = eTodoDB:readUserCfg(User),
    ExtUsers  = getUsersWithEmail(UserCfg#userCfg.ownerCfg),
    ExtUsers2 = lists:sort(fun(X, Y) ->
                               string:to_lower(X) < string:to_lower(Y)
                           end,
                           ExtUsers ++ OffLineUsers),
    [wxCheckListBox:append(EmailCheckBox, ExtUser) || ExtUser <- ExtUsers2].


addIfEmail(Peer, Checked, State) ->
    EmailCheckBox = obj("emailCheckBox", State),
    case eTodoDB:getConnection(Peer) of
        #conCfg{email = undefined} -> ok;
        #conCfg{email = ""}        -> ok;
        #conCfg{email = _Email} ->
            Index = wxCheckListBox:getCount(EmailCheckBox),
            wxCheckListBox:append(EmailCheckBox, Peer),
            wxCheckListBox:check(EmailCheckBox, Index, [{check, Checked}])
    end.

getUsersWithEmail(undefined) -> [];
getUsersWithEmail(Users) ->
    getUsersWithEmail(Users, []).

getUsersWithEmail([], Acc) ->
    lists:reverse(Acc);
getUsersWithEmail([User|Rest], Acc) ->
    case eTodoUtils:getPeerInfo(User) of
        {UserWithEmail, _Email} ->
            getUsersWithEmail(Rest, [UserWithEmail|Acc]);
        _ ->
            getUsersWithEmail(Rest, Acc)
    end.

%%======================================================================
%% Function : updateMsgWindow(State, User) -> NewState
%% Purpose  : Update msg window.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateMsgWindow(State, _User) ->
    loadMessages(obj("msgTextWin", State), State),
    State.

clearMsgCounter(State = #guiState{unreadMsgs2 = {C1, C2, C3}}) ->
    {Counters, State2} =
        case msgKind() of
            msgEntry   -> {{0, C2, C3}, clearNotificationTimer(State)};
            alarmEntry -> {{C1, 0, C3}, State};
            _System    -> {{C1, C2, 0}, State}
        end,
    case Counters of
        {C1, C2, C3} ->
            State2;
        _ ->
            saveMsgCounters(
              showMsgCounters(State2#guiState{unreadMsgs2 = Counters}))
    end.

clearNotificationTimer(State = #guiState{notificationTimer = undefined}) ->
    State;
clearNotificationTimer(State = #guiState{notificationTimer = TimerRef})  ->
    erlang:cancel_timer(TimerRef),
    State#guiState{notificationTimer = undefined}.

setMsgCounter(Notebook, 0) ->
    wxNotebook:setPageImage(Notebook, 1, 3),
    wxNotebook:setPageText(Notebook, 1, ?tr("messagePanel"));
setMsgCounter(Notebook, Num) ->
    wxNotebook:setPageImage(Notebook, 1, 4),
    wxNotebook:setPageText(Notebook, 1,
                           ?tr("messagePanel") ++ "(" ++ integer_to_list(Num) ++ ")").

%%======================================================================
%% Function : chatMsgStatusBar(MsgType, Msg, State) -> ok
%% Purpose  : Add message to status bar if "Messages" isn't active.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
chatMsgStatusBar(MsgType, Msg, State) ->
    case messagesVisible(State) andalso msgKind() =:= MsgType of
        true ->
            clearStatusBar(State);
        false ->
            State2 = increaseMsgCounter(State, obj("mainNotebook", State)),
            State3 = increaseKindCounter(MsgType, State2),
            StatusBarObj = obj("mainStatusBar", State3),
            wxStatusBar:setStatusText(StatusBarObj, Msg, [{number, 2}]),
            saveMsgCounters(State3)
    end.

increaseMsgCounter(State = #guiState{unreadMsgs = Before}, Notebook) ->
    setMsgCounter(Notebook, Before + 1),
    wxNotebook:setPageImage(Notebook, 1, 4),
    State#guiState{unreadMsgs = Before + 1}.

increaseKindCounter(msgEntry,
                    State = #guiState{unreadMsgs2 = {C1, C2, C3}}) ->
    clearNotificationTimer(State),
    NotificationTime = getNotificationTime(State#guiState.user),
    TimerRef = erlang:send_after(NotificationTime, self(), sendNotification),
    State#guiState{unreadMsgs2 = {C1 + 1, C2, C3}, notificationTimer = TimerRef};
increaseKindCounter(alarmEntry,
                    State = #guiState{unreadMsgs2 = {C1, C2, C3}}) ->
    State#guiState{unreadMsgs2 = {C1, C2 + 1, C3}};
increaseKindCounter(systemEntry,
                    State = #guiState{unreadMsgs2 = {C1, C2, C3}}) ->
    State#guiState{unreadMsgs2 = {C1, C2, C3 + 1}}.

getNotificationTime(User) ->
    UserCfg = eTodoDB:readUserCfg(User),
    Saved = default(UserCfg#userCfg.notificationTime, ?NotificationTime),
    application:get_env(eTodo, notificationTime, Saved) * 1000.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setUnreadMsgs(UserName, State) ->
    UserCfg  = eTodoDB:readUserCfg(UserName),
    Counters = case UserCfg#userCfg.unreadMsgs2 of
                   {C1, C2, C3} when is_integer(C1), is_integer(C2),
                                     is_integer(C3) -> {C1, C2, C3};
                   _ -> {0, 0, 0}
               end,
    State2   = showMsgCounters(State#guiState{unreadMsgs2 = Counters}),
    Notebook = obj("mainNotebook", State2),
    case wxNotebook:getCurrentPage(Notebook) =:=
        wxNotebook:getPage(Notebook, 1) of
        true  -> clearMsgCounter(clearStatusBar(State2));
        false -> State2
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
eTodoUpdate(Url, State) ->
    Opt = [{ssl, httpc:ssl_verify_host_options(true)}],
    case httpc:request(get, {"https://" ++ Url, ""}, Opt, []) of
        {ok, {{_, 200, _}, _Headers, ETodoLib}} ->
            eTodoUpdateGUI(ETodoLib, State);
        _ ->
            eTodo:systemEntry(system, "Failed to download update."),
            State
    end.

eTodoUpdateGUI(ETodoLib, State = #guiState{frame = Frame}) ->
    Dlg = wxMessageDialog:new(Frame, "Do you want to update eTodo?",
                              [{caption, "Update eTodo"},
                               {style,   ?wxYES_NO}]),
    case wxMessageDialog:showModal(Dlg) of
        ?wxID_YES ->
            wxMessageDialog:destroy(Dlg),
            doUpdateETodo(ETodoLib, State);
        _ ->
            wxMessageDialog:destroy(Dlg),
            State
    end.

doUpdateETodo(ETodoLib, State = #guiState{frame = Frame}) ->
    {ok, Cwd} = file:get_cwd(),
    InstallDir = filename:dirname(code:lib_dir(eTodo)),
    file:set_cwd(InstallDir),
    file:write_file("eTodo.zip", ETodoLib),
    zip:unzip("eTodo.zip"),
    file:delete("eTodo.zip"),
    file:set_cwd(Cwd),
    MsgDlg = wxMessageDialog:new(Frame, "Restart eTodo to complete update",
                                 [{caption, "Restart eTodo"}]),
    wxDialog:showModal(MsgDlg),
    wxDialog:destroy(MsgDlg),
    State.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
convertToLocal(CT) ->
    case catch wx:get_env() of
        {'EXIT', _} ->
            %% Should only convert images when wx is available.
            CT;
        _ ->
            Opt = [{ssl, httpc:ssl_verify_host_options(true)}],
            case httpc:request(get, {binary_to_list(CT), ""}, Opt, []) of
                {ok, {{_, 200, _}, Headers, Body}} ->
                    CType = proplists:get_value("content-type", Headers, undef),
                    convertToLocal(CType, Body, CT);
                _ ->
                    eTodo:systemEntry(system, "Failed to download image."),
                    CT
            end
    end.

convertToLocal(CType, Img, CT) when (CType == "image/jpeg") or
                                    (CType == "image/png")  or
                                    (CType == "image/bmp")  or
                                    (CType == "image/gif")  ->
    FileName = filename:join([eTodoUtils:getUserCfgDir(), "linkedFiles",
                              integer_to_list(erlang:phash2(CT)) ++ ".png"]),
    saveFile(FileName, Img, CT);
convertToLocal(_ContentType, _Img, CT) ->
    eTodo:systemEntry(system, "Failed to get image, no support for image type."),
    CT.

saveFile(FileName, Img, CT) ->
    file:write_file(FileName, Img),
    wx:get_env(),
    Image   = wxImage:new(FileName),
    Width   = wxImage:getWidth(Image),
    Height  = wxImage:getHeight(Image),
    Options = [{quality, ?wxIMAGE_QUALITY_HIGH}],
    Bitmap  = case Width > 1024 of
                  true ->
                      NewHeight = round(1024/Width * Height),
                      wxBitmap:new(wxImage:scale(Image, 1024, NewHeight, Options));
                  false ->
                      wxBitmap:new(Image)
              end,
    case wxBitmap:saveFile(Bitmap, FileName, ?wxBITMAP_TYPE_PNG) of
        true ->
            {CT, list_to_binary(FileName)};
        false ->
            CT
    end.

%%====================================================================
%% Work around for crashing unicode input.
%%====================================================================
setText(Obj, "") ->
    wxStyledTextCtrl:setTextRaw(Obj, <<0>>);
setText(Obj, Text) ->
    wxStyledTextCtrl:setReadOnly(Obj, false),
    Utf8Bin = filterAndConvert2Utf8(Text),
    wxStyledTextCtrl:setTextRaw(Obj, <<Utf8Bin/binary, 0>>).

setItem(List, Row, Col, Text) ->
    case catch wxListCtrl:setItem(List, Row, Col, Text) of
        {'EXIT', _} ->
            Text2 = filterAndConvert2Utf8Text(lists:flatten(Text)),
            wxListCtrl:setItem(List, Row, Col, Text2);
        _ ->
            ok
    end.

filterAndConvert2Utf8Text(Text) ->
    Utf8Bin = filterAndConvert2Utf8(Text),
    unicode:characters_to_list(Utf8Bin).

filterAndConvert2Utf8(Text) ->
    filterUnicode(Text, <<>>).

filterUnicode(Text, SoFar) ->
    case unicode:characters_to_binary(Text) of
        {error, Bin, Rest} when is_binary(Rest) ->
            <<_:8, Rest2/binary>> = Rest,
            filterUnicode(Rest2, <<SoFar/binary, Bin/binary>>);
        {error, Bin, Rest} ->
            filterUnicode(tl(Rest), <<SoFar/binary, Bin/binary>>);
        {incomplete, Bin, _Rest} ->
            <<SoFar/binary, Bin/binary>>;
        Bin ->
            <<SoFar/binary, Bin/binary>>
    end.