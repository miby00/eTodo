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

-include_lib("eTodo/include/eTodo.hrl").

-include_lib("wx/include/wx.hrl").

%% API
-export([
         addTodo/4,
         appendToPage/4,
         appendToPage/6,
         chatMsgStatusBar/3,
         checkStatus/1,
         checkUndoStatus/1,
         clearAndInitiate/2,
         clearMsgCounter/1,
         clearStatusBar/1,
         convertToLocal/1,
         date2wxDate/1,
         deleteAndUpdate/3,
         doLogout/2,
         eTodoUpdate/2,
         findSelected/1,
         focusAndSelect/1,
         focusAndSelect/2,
         generateSchedule/1,
         generateTimeLog/1,
         generateWorkLog/1,
         getCheckedItems/1,
         getPortrait/1,
         getTaskList/1,
         getTodoList/2,
         getTodoLists/1,
         getWorkDesc/2,
         makeETodo/3,
         obj/2,
         pos/2,
         saveColumnSizes/1,
         saveMsg/2,
         setColor/2,
         setColumnWidth/4,
         setDoneTimeStamp/3,
         setOwner/3,
         setPeerStatusIfNeeded/1,
         setPortrait/2,
         setScrollBar/1,
         setSelection/1,
         setSelection/2,
         setSelection/4,
         setTaskLists/2,
         setUnreadMsgs/2,
         showBookmarkMenu/2,
         showMenu/4,
         toClipboard/1,
         toClipboard/2,
         type/1,
         updateGui/3,
         updateGui/4,
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

appendToPage(MsgObj, Type, From, To, {Html, _HtmlCSS}, State) ->
    State3 = case evalShow(Type, From, To, State#guiState.msgCfg) of
                 true ->
                     wxHtmlWindow:appendToPage(MsgObj, Html),
                     State;
                 false ->
                     Notebook1 = obj("mainNotebook",   State),
                     Notebook2 = obj("msgWinNotebook", State),
                     State2 = increaseMsgCounter(State, Notebook1),
                     increaseMsgCounter(Type, State2, Notebook2)
             end,
    eTodoDB:appendToPage(State3#guiState.user, Type, From, To, Html),
    setScrollBar(MsgObj),
    State3.

evalShow(_, UserName, To, {true, Users}) ->
    lists:member(UserName, Users) or (Users =/= (Users -- To));
evalShow(_,  _From, _To, _msgCfg)       -> true.

setScrollBar(MsgObj) ->
    Range = wxHtmlWindow:getScrollRange(MsgObj, ?wxVERTICAL),
    wxHtmlWindow:scroll(MsgObj, 0, Range).

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
    StatusBar = obj("mainStatusBar", State),
    wxStatusBar:setStatusText(StatusBar, User,           [{number, 0}]),
    wxStatusBar:setStatusText(StatusBar, ?sbNotLoggedIn, [{number, 1}]),
    userStatusOffline(State),

    ePeerEM:loggedOut(User),
    eWeb:stop(),
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
            case wxXmlResource:xrcctrl(Frame, Name, type(Name)) of
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
    updateValue("descriptionArea",   State, ETodo#etodo.description),
    updateValue("sharedWithText",    State, ETodo#etodo.sharedWith),
    updateValue("addedToLists",      State, ETodo#etodo.lists),
    updateValue("commentArea",       State, ETodo#etodo.comment),
    doSetSelection("statusChoice",   State, ETodo#etodo.status),
    doSetSelection("priorityChoice", State, ETodo#etodo.priority),
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
        {wxTextCtrl, Name} when
              (Name == "descriptionArea") or
              (Name == "commentArea")     ->
            %% Do not generate update event when showing new eTodo, only
            %% when the user edits the field.
            wxTextCtrl:changeValue(Obj, Value);
        {wxTextCtrl, Name} ->
            wxTextCtrl:setValue(Obj, Value);
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

    updateValue("descriptionArea", State, Desc,     OldDesc),
    updateValue("sharedWithText",  State, Share,    OldShare),
    updateValue("addedToLists",    State, Lists,    OldLists),
    updateValue("commentArea",     State, Comment,  OldComment),
    setSelection("statusChoice",   State, Status,   OldStatus),
    setSelection("priorityChoice", State, Priority, OldPriority),
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
            wxListCtrl:setItem(List, Row, UidCol,        Uid),
            wxListCtrl:setItem(List, Row, StatusCol,     Status),
            wxListCtrl:setItem(List, Row, OwnerCol,      Owner),
            wxListCtrl:setItem(List, Row, PriorityCol,   Priority),
            wxListCtrl:setItem(List, Row, DueTimeCol,    DueTime),
            wxListCtrl:setItem(List, Row, DescCol,       colFormat(Desc)),
            wxListCtrl:setItem(List, Row, CommentCol,    colFormat(Comment)),
            wxListCtrl:setItem(List, Row, SharedWithCol, SharedWith),
            wxListCtrl:setItem(List, Row, CreateTimeCol, CreateTime),
            wxListCtrl:setItem(List, Row, DoneTimeCol,   DoneTime),

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
    case Row rem 2 of
        0 ->
            Color = {230, 230, 230, 255},
            wxListCtrl:setItemBackgroundColour(List, Row, Color);
        _ ->
            Color = {255, 255, 255, 255},
            wxListCtrl:setItemBackgroundColour(List, Row, Color)
    end.

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

    wxListCtrl:freeze(TodoList),
    clearAndInitiate(TodoList, length(ETodos)),
    lists:foldl(fun (ETodo, Acc) ->
                        setRowInfo(TodoList, ETodo, Acc),
                        Acc + 1
                end, 0, ETodos),
    wxListCtrl:thaw(TodoList),

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

    Reminder = obj("setReminderButton", State),
    Shared   = obj("shareButton",       State),
    TaskEdit = obj("taskEditPanel",     State),

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
    CHtmlWin     = obj("msgTextWin",    State),
    RHtmlWin     = obj("remTextWin",    State),
    SHtmlWin     = obj("systemTextWin", State),
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
            eWeb:setStatusUpdate(User, Status, StatusMsg),
            Users = [UserStatus#userStatus.userName || UserStatus <- UserList],
            ePeerEM:sendMsg(User, Users, statusEntry,
                            {statusUpdate, StatusUpdate, getPortrait(User)}),
            MsgTop = obj("msgTopPanel", State),
            wxPanel:layout(MsgTop),
            wxPanel:refresh(MsgTop),
            setScrollBar(CHtmlWin),
            setScrollBar(RHtmlWin),
            setScrollBar(SHtmlWin),
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

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj2),

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

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj4),
    wxStaticBitmap:show(Obj3),

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

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:show(Obj1),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(UserStatus = #userStatus{status = "Busy"}, State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj3),
    wxStaticBitmap:show(Obj2),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(UserStatus = #userStatus{status = "Away"}, State) ->
    Obj1 = obj("peerAvailableIcon", State),
    Obj2 = obj("peerBusyIcon",      State),
    Obj3 = obj("peerAwayIcon",      State),

    %% Set correct icon to show
    wxStaticBitmap:hide(Obj1),
    wxStaticBitmap:hide(Obj2),
    wxStaticBitmap:show(Obj3),

    setPeerStatusTextAndLayout(UserStatus, State);
setPeerStatus(_UserStatus, State) ->
    State.

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
                       getRootDir() ++ "/Icons/portrait.png"
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
    Obj   = obj("userCheckBox", State),
    Index = wxCheckListBox:getSelection(Obj),
    case Index >= 0 of
        true ->
            User  = wxCheckListBox:getString(Obj, Index),
            case lists:keyfind(User, #userStatus.userName, Users) of
                UserStatus when is_record(UserStatus, userStatus) ->
                    setPeerStatus(UserStatus, State);
                false ->
                    clearPeerStatus(State)
            end;
        false ->
            ok
    end,
    State.

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

    Report    = eHtml:makeWorkLogReport(User, Date),
    wxHtmlWindow:setPage(Obj2, Report),
    State.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
generateTimeLog(State = #guiState{user = User}) ->
    Obj      = obj("timeLogReport", State),
    TaskList = getTaskList(State),
    AllTask  = TaskList == ?defTaskList,
    Report   = eHtml:makeTimeLogReport(User, State#guiState.rows, AllTask),
    wxHtmlWindow:setPage(Obj, Report),
    State.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
generateSchedule(State = #guiState{user = User}) ->
    Obj    = obj("scheduleReport", State),
    Report = eHtml:makeScheduleReport(User),
    wxHtmlWindow:setPage(Obj, Report),
    State.

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
saveMsg(UserName, State) ->
    UserCfg  = eTodoDB:readUserCfg(UserName),
    Unread   = default(UserCfg#userCfg.unreadMsgs, 0),
    Notebook = obj("mainNotebook",  State),
    CurrPage = wxNotebook:getCurrentPage(Notebook),
    MsgPage  = wxNotebook:getPage(Notebook, 1),
    UserCfg2 = case CurrPage of
                   MsgPage ->
                       UserCfg#userCfg{unreadMsgs = 0};
                   _ when is_integer(Unread) ->
                       Unread2 = Unread + 1,
                       UserCfg#userCfg{unreadMsgs = Unread2};
                   _ when is_list(Unread) ->
                       Unread2 = length(Unread) + 1,
                       UserCfg#userCfg{unreadMsgs = Unread2}
               end,
    eTodoDB:saveUserCfg(UserCfg2).

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
%% Function : updateMsgWindow(State, User) -> NewState
%% Purpose  : Update msg window.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateMsgWindow(State, User) ->
    CHtmlWin  = obj("msgTextWin",    State),
    RHtmlWin  = obj("remTextWin",    State),
    SHtmlWin  = obj("systemTextWin", State),
    MsgTop    = obj("msgTopPanel",  State),

    Chats     = getMessages(User, State),
    Reminders = eTodoDB:getMessages(User, [alarmEntry]),
    System    = eTodoDB:getMessages(User, [systemEntry]),

    wxHtmlWindow:setPage(CHtmlWin, Chats),
    wxHtmlWindow:setPage(RHtmlWin, Reminders),
    wxHtmlWindow:setPage(SHtmlWin, System),

    wxPanel:layout(MsgTop),
    wxPanel:refresh(MsgTop),
    setScrollBar(CHtmlWin),
    setScrollBar(RHtmlWin),
    setScrollBar(SHtmlWin),
    State.

getMessages(User, #guiState{msgCfg = {true, Users}}) ->
    eTodoDB:getMessagesFromOrTo(User, Users);
getMessages(User, #guiState{msgCfg = {false, _Users}}) ->
    eTodoDB:getMessages(User, [msgEntry]).

clearMsgCounter(State = #guiState{user = User,
                                  unreadMsgs2 = {C1, C2, C3}}) ->
    Notebook1 = obj("mainNotebook",  State),
    Notebook2 = obj("msgWinNotebook", State),
    ChatPanel = wxNotebook:getPage(Notebook2, 0),
    RemPanel  = wxNotebook:getPage(Notebook2, 1),
    SysPanel  = wxNotebook:getPage(Notebook2, 2),

    State2 =
        case wxNotebook:getCurrentPage(Notebook2) of
            ChatPanel ->
                CTot2 = C2 + C3,
                setMsgCounter(Notebook1, CTot2),
                wxNotebook:setPageText(Notebook2, 0, "Chat"),
                NState = clearNotificationTimer(State),
                NState#guiState{unreadMsgs = CTot2, unreadMsgs2 = {0, C2, C3}};
            RemPanel ->
                CTot2 = C1 + C3,
                setMsgCounter(Notebook1, CTot2),
                wxNotebook:setPageText(Notebook2, 1, "Reminder"),
                State#guiState{unreadMsgs = CTot2, unreadMsgs2 = {C1, 0, C3}};
            SysPanel ->
                CTot2 = C1 + C2,
                setMsgCounter(Notebook1, CTot2),
                wxNotebook:setPageText(Notebook2, 2, "System"),
                State#guiState{unreadMsgs = CTot2, unreadMsgs2 = {C1, C2, 0}}
        end,

    UserCfg = eTodoDB:readUserCfg(User),
    {NC1, NC2, NC3} = State2#guiState.unreadMsgs2,
    eTodoDB:saveUserCfg(UserCfg#userCfg{unreadMsgs = NC1 + NC2 + NC3,
                                        unreadMsgs2 = {NC1, NC2, NC3}}),
    State2.

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
    Notebook1 = obj("mainNotebook", State),
    CurrPage1 = wxNotebook:getCurrentPage(Notebook1),
    MsgPage1  = wxNotebook:getPage(Notebook1, 1),
    Notebook2 = obj("msgWinNotebook", State),
    CurrPage2 = wxNotebook:getCurrentPage(Notebook2),
    MsgPage2  = getMessagePage(MsgType, Notebook2),
    case CurrPage1 of
        MsgPage1 ->
            case CurrPage2 of
                MsgPage2 ->
                    clearStatusBar(State);
                _ ->
                    State2 = increaseMsgCounter(State, Notebook1),
                    State3 = increaseMsgCounter(MsgType, State2, Notebook2),
                    StatusBarObj = obj("mainStatusBar", State3),
                    wxStatusBar:setStatusText(StatusBarObj, Msg, [{number, 2}]),
                    State3
            end;
        _ ->
            State2 = increaseMsgCounter(State, Notebook1),
            State3 = increaseMsgCounter(MsgType, State2, Notebook2),
            StatusBarObj = obj("mainStatusBar", State3),
            wxStatusBar:setStatusText(StatusBarObj, Msg, [{number, 2}]),
            State3
    end.

getMessagePage(msgEntry, Notebook) ->
    wxNotebook:getPage(Notebook, 0);
getMessagePage(alarmEntry, Notebook) ->
    wxNotebook:getPage(Notebook, 1);
getMessagePage(systemEntry, Notebook) ->
    wxNotebook:getPage(Notebook, 2).

increaseMsgCounter(State = #guiState{unreadMsgs = Before}, Notebook) ->
    setMsgCounter(Notebook, Before + 1),
    wxNotebook:setPageImage(Notebook, 1, 4),
    State#guiState{unreadMsgs = Before + 1}.

increaseMsgCounter(msgEntry,
                   State = #guiState{unreadMsgs2 = {C1, C2, C3}},
                   Notebook) ->
    clearNotificationTimer(State),
    NotificationTime = getNotificationTime(State#guiState.user),
    TimerRef = erlang:send_after(NotificationTime, self(), sendNotification),
    PageText = "Chat(" ++ integer_to_list(C1 + 1) ++ ")",
    wxNotebook:setPageText(Notebook, 0, PageText),
    State#guiState{unreadMsgs2 = {C1 + 1, C2, C3}, notificationTimer = TimerRef};
increaseMsgCounter(alarmEntry,
                   State = #guiState{unreadMsgs2 = {C1, C2, C3}},
                   Notebook) ->
    PageText = "Reminder(" ++ integer_to_list(C2 + 1) ++ ")",
    wxNotebook:setPageText(Notebook, 1, PageText),
    State#guiState{unreadMsgs2 = {C1, C2 + 1, C3}};
increaseMsgCounter(systemEntry,
                   State = #guiState{unreadMsgs2 = {C1, C2, C3}},
                   Notebook) ->
    PageText = "System(" ++ integer_to_list(C3 + 1) ++ ")",
    wxNotebook:setPageText(Notebook, 2, PageText),
    State#guiState{unreadMsgs2 = {C1, C2, C3 + 1}}.

getNotificationTime(User) ->
    UserCfg = eTodoDB:readUserCfg(User),
    default(UserCfg#userCfg.notificationTime, ?NotificationTime) * 1000.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
setUnreadMsgs(UserName, State) ->
    Notebook = obj("mainNotebook",  State),
    CurrPage = wxNotebook:getCurrentPage(Notebook),
    MsgPage  = wxNotebook:getPage(Notebook, 1),
    case CurrPage of
        MsgPage ->
            clearStatusBar(State);
        _ ->
            UserCfg    = eTodoDB:readUserCfg(UserName),
            UnreadMsgs = UserCfg#userCfg.unreadMsgs,
            case  UnreadMsgs > 0 of
                true ->
                    Notebook = obj("mainNotebook",  State),
                    wxNotebook:setPageText(Notebook, 1,
                                           "Messages(" ++ integer_to_list(UnreadMsgs) ++ ")");
                false ->
                    ok
            end,
            State#guiState{unreadMsgs = UnreadMsgs}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
eTodoUpdate(Url, State) ->
    Opt = [{ssl, [{verify, verify_none}]}],
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
    WXAvailable =
        case catch wx:get_env() of
            {'EXIT', _} ->
                %% Should only convert images when wx is available.
                false;
            _ ->
                true
        end,
    Opt = [{ssl, [{verify, verify_none}]}],
    case httpc:request(get, {binary_to_list(CT), ""}, Opt, []) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            CType = proplists:get_value("content-type", Headers, undef),
            convertToLocal(CType, Body, CT, WXAvailable);
        _ ->
            if WXAvailable ->
                    eTodo:systemEntry(system, "Failed to download image.");
               true ->
                    ok
            end,
            CT
    end.

convertToLocal(CType, Img, CT, WX) when (CType == "image/jpeg") or
                                        (CType == "image/png")  or
                                        (CType == "image/bmp")  or
                                        (CType == "image/gif")  ->
    FileName = filename:join([eTodoUtils:getRootDir(),
                              "www", "linkedFiles",
                              integer_to_list(erlang:phash2(CT))]),
    saveFile(CType, FileName, Img, CT, WX);
convertToLocal(_ContentType, _Img, CT, true) ->
    eTodo:systemEntry(system, "Failed to get image, no support for image type."),
    CT;
convertToLocal(_ContentType, _Img, CT, false) ->
    CT.

saveFile(_CType, File, Img, CT, true) ->
    FileName = File ++ ".png",
    file:write_file(FileName, Img),
    wx:get_env(),
    Image   = wxImage:new(FileName),
    Width   = wxImage:getWidth(Image),
    Height  = wxImage:getHeight(Image),
    Options = [{quality, ?wxIMAGE_QUALITY_HIGH}],
    Bitmap = case Width > 1024 of
                 true ->
                     NewHeight = round(1024/Width * Height),
                     wxBitmap:new(wxImage:scale(Image, 1024, NewHeight, Options));
                 false ->
                     wxBitmap:new(Image)
             end,
    case wxBitmap:saveFile(Bitmap, FileName, ?wxBITMAP_TYPE_PNG) of
        true ->
            list_to_binary(FileName);
        false ->
            CT
    end;
saveFile("image/jpeg", File, Img, _CT, false) ->
    FileName = File ++ ".jpg",
    file:write_file(FileName, Img),
    list_to_binary(FileName);
saveFile("image/png", File, Img, _CT, false) ->
    FileName = File ++ ".png",
    file:write_file(FileName, Img),
    list_to_binary(FileName);
saveFile("image/bmp", File, Img, _CT, false) ->
    FileName = File ++ ".bmp",
    file:write_file(FileName, Img),
    list_to_binary(FileName);
saveFile("image/gif", File, Img, _CT, false) ->
    FileName = File ++ ".gif",
    file:write_file(FileName, Img),
    list_to_binary(FileName);
saveFile(_CType, _File, _Img, CT, _) ->
    CT.
