%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(eTodo).
-author("mikael.bylund@gmail.com").

-include_lib("wx/include/wx.hrl").

-include("eTodo.hrl").

-behaviour(wx_object).

%% API
-export([acceptingIncCon/3,
         alarmEntry/2,
         checkConflict/5,
         delayedUpdateGui/2,
         getSearchCfg/0,
         getFilterCfg/1,
         getTimeReportData/0,
         loggedIn/1,
         loggedOut/1,
         msgEntry/3,
         start/0,
         start/1,
         stop/0,
         systemEntry/2,
         taskListDeleted/1,
         taskListsUpdated/0,
         todoCreated/3,
         todoUpdated/2,
         todoDeleted/1,
         writing/1,
         statusUpdate/2,

         launchBrowser/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(eTodoUtils, [cancelTimer/1,
                     convertUid/1,
                     default/2,
                     getRootDir/0,
                     taskInternal/1,
                     toDB/1,
                     toStr/2,
                     toStr/1]).

-import(eGuiFunctions, [addTodo/4,
                        appendToPage/2,
                        appendToReminders/2,
                        checkStatus/1,
                        checkUndoStatus/1,
                        clearStatusBar/1,
                        deleteAndUpdate/3,
                        doLogout/2,
                        focusAndSelect/2,
                        getPortrait/1,
                        getTaskList/1,
                        getTodoList/2,
                        getTodoLists/1,
                        makeETodo/3,
                        obj/2,
                        pos/2,
                        saveColumnSizes/1,
                        saveMsg/5,
                        setColumnWidth/4,
                        setDoneTimeStamp/3,
                        setOwner/3,
                        setPeerStatusIfNeeded/1,
                        setSelection/1,
                        setSelection/2,
                        setSelection/4,
                        setTaskLists/2,
                        type/1,
                        updateGui/3,
                        updateGui/4,
                        updateTodo/4,
                        updateTodoInDB/2,
                        updateTodoWindow/1,
                        updateValue/4,
                        useFilter/3,
                        userStatusUpdate/1,
                        date2wxDate/1,
                        wxDate2Date/1,
                        xrcId/1
                       ]).

-import(eRows, [findIndex/2,
                getETodoAtIndex/2,
                insertRow/3]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    wx_object:start({local, ?MODULE}, ?MODULE, [], []).

start(Arg) ->
    wx_object:start({local, ?MODULE}, ?MODULE, [Arg], []).

stop() ->
    wx_object:call(?MODULE, stop).

getSearchCfg() ->
    wx_object:call(?MODULE, getSearchCfg).

getFilterCfg(TaskList) ->
    wx_object:call(?MODULE, {getFilterCfg, TaskList}).

getTimeReportData() ->
    wx_object:call(?MODULE, getTimeReportData).

launchBrowser(URL) ->
    wx_object:cast(?MODULE, {launchBrowser, URL}), ok.

acceptingIncCon(User, Circle, Port) ->
    wx_object:cast(?MODULE, {acceptingIncCon, User, Circle, Port}), ok.

delayedUpdateGui(ETodo, Index) ->
    wx_object:cast(?MODULE, {delayedUpdateGui, ETodo, Index}), ok.

msgEntry(User, Users, Text) ->
    wx_object:cast(?MODULE, {msgEntry, User, Users, Text}), ok.

systemEntry(Uid, Text) ->
    wx_object:cast(?MODULE, {systemEntry, Uid, Text}), ok.

alarmEntry(Uid, Text) ->
    wx_object:cast(?MODULE, {alarmEntry, Uid, Text}), ok.

loggedIn(User) ->
    wx_object:cast(?MODULE, {loggedIn, User}), ok.

loggedOut(User) ->
    wx_object:cast(?MODULE, {loggedOut, User}), ok.

writing(Sender) ->
    wx_object:cast(?MODULE, {writing, Sender}), ok.

statusUpdate(UserStatus, Avatar) ->
    wx_object:cast(?MODULE, {statusUpdate, UserStatus, Avatar}), ok.

taskListsUpdated() ->
    wx_object:cast(?MODULE, taskListsUpdated), ok.

taskListDeleted(List) ->
    wx_object:cast(?MODULE, {taskListDeleted, List}), ok.

todoUpdated(Sender, Todo) ->
    wx_object:cast(?MODULE, {todoUpdated, Sender, Todo}), ok.

todoCreated(TaskList, Row, Todo) ->
    wx_object:cast(?MODULE, {todoCreated, TaskList, Row, Todo}), ok.

todoDeleted(Uid) ->
    wx_object:cast(?MODULE, {todoDeleted, Uid}), ok.

checkConflict(User, PeerUser, OldLocalConTime, OldRemoteConTime, Diff) ->
    wx_object:cast(?MODULE, {checkConflict, User, PeerUser,
                             OldLocalConTime, OldRemoteConTime, Diff}), ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    init([normal]);
init([Arg]) ->
    {ok, WorkDir} = file:get_cwd(),

    process_flag(trap_exit, true),
    eLog:log(debug, ?MODULE, init, [Arg],
             "##### eTodo started #####", ?LINE),

    case os:getenv("eTodoMode") of
        false ->
            %% Change dir while building GUI to allow wx to find icons.
            file:set_cwd(code:priv_dir(?MODULE)),
            {WXFrame, State} = initGUI(Arg),
            eLang:initiateGUI(State),
            file:set_cwd(WorkDir),
            {WXFrame, State};
        _ ->
            User      = os:getenv("eTodoUser"),
            Pwd       = os:getenv("eTodoPwd"),
            Circle    = os:getenv("eTodoCircle"),
            %% Add handler to receive events
            ePeerEM:add_handler(eTodoEH, {User, noGui}),
            ePeerEM:connectToCircle(User, Circle, Pwd),
            eTodoAlarm:loggedIn(User),
            %% Start wx so eTodo can receive events.
            wx:new(),
            {wxFrame:new(), #guiState{mode = noGui,
                                      user = User}}
    end.

initGUI(Arg) ->
    Dict = #{},
    WX   = wx:new(),
    eLang:start_link(wx:get_env()),

    %% Do not use mac native list ctrl, auto sort is bad for eTodo.
    (catch wxSystemOptions:setOption("mac.listctrl.always_use_generic", 1)),

    Frame     = wxFrame:new(),
    Login     = wxDialog:new(),
    Time      = wxDialog:new(),
    Timer     = wxDialog:new(),
    Search    = wxDialog:new(),
    Plugins   = wxDialog:new(),
    LogWork   = wxDialog:new(),
    Users     = wxDialog:new(),
    AddList   = wxDialog:new(),
    Settings  = wxDialog:new(),
    ManLists  = wxDialog:new(),
    ManOwner  = wxDialog:new(),
    ManBookm  = wxDialog:new(),
    SortCols  = wxDialog:new(),
    Conflict  = wxDialog:new(),
    About     = wxDialog:new(),
    MsgMenu   = wxMenu:new(),
    Xrc       = wxXmlResource:get(),

    %% Add menu choice
    wxMenu:append(MsgMenu, ?clearMsg, ?clearMsgText),
    wxMenu:append(MsgMenu, ?clearRem, ?clearRemText),
    wxMenu:connect(MsgMenu, command_menu_selected),

    wxXmlResource:initAllHandlers(Xrc),
    wxXmlResource:load(Xrc, "eTodo.xrc"),

    %% Load and construct main window from xml file
    wxXmlResource:loadFrame(Xrc,   Frame,    WX,    "mainFrame"),
    wxXmlResource:loadDialog(Xrc,  Login,    Frame, "loginDlg"),
    wxXmlResource:loadDialog(Xrc,  Time,     Frame, "reminderDlg"),
    wxXmlResource:loadDialog(Xrc,  Timer,    Frame, "timerDlg"),
    wxXmlResource:loadDialog(Xrc,  Search,   Frame, "searchDlg"),
    wxXmlResource:loadDialog(Xrc,  Plugins,  Frame, "pluginDlg"),
    wxXmlResource:loadDialog(Xrc,  LogWork,  Frame, "logWorkDlg"),
    wxXmlResource:loadDialog(Xrc,  Users,    Frame, "userDlg"),
    wxXmlResource:loadDialog(Xrc,  AddList,  Frame, "listDlg"),
    wxXmlResource:loadDialog(Xrc,  Settings, Frame, "settingsDlg"),
    wxXmlResource:loadDialog(Xrc,  ManLists, Frame, "manageListDlg"),
    wxXmlResource:loadDialog(Xrc,  ManOwner, Frame, "manageOwnerDlg"),
    wxXmlResource:loadDialog(Xrc,  ManBookm, Frame, "manageBookmarkDlg"),
    wxXmlResource:loadDialog(Xrc,  SortCols, Frame, "sortColumnsDlg"),
    wxXmlResource:loadDialog(Xrc,  Conflict, Frame, "conflictDlg"),
    wxXmlResource:loadDialog(Xrc,  About,    Frame, "aboutDlg"),

    setIcon(Frame),
    setIcon(Login),
    setIcon(Time),
    setIcon(Timer),
    setIcon(Search),
    setIcon(Plugins),
    setIcon(LogWork),
    setIcon(Users),
    setIcon(AddList),
    setIcon(Settings),
    setIcon(ManLists),
    setIcon(ManOwner),
    setIcon(ManBookm),
    setIcon(SortCols),
    setIcon(Conflict),
    setIcon(About),

    IL         = wxImageList:new(16,16),
    MultiPng   = wxImage:new("./Icons/multi.png"),
    MultiBmp   = wxBitmap:new(MultiPng),
    SinglePng  = wxImage:new("./Icons/single.png"),
    SingleBmp  = wxBitmap:new(SinglePng),
    CheckedPng = wxImage:new("./Icons/task-complete.png"),
    CheckedBmp = wxBitmap:new(CheckedPng),
    wxImageList:add(IL, SingleBmp),
    wxImageList:add(IL, MultiBmp),
    wxImageList:add(IL, CheckedBmp),

    DefUserCfg = eTodoDB:readUserCfg(default),
    DefUser    = setUserNameAndFocus(Login, DefUserCfg),
    wxFrame:setTitle(Frame, "eTodo - " ++ DefUser ++ " (not logged in)"),

    Event1 = command_checkbox_clicked,
    Event2 = date_changed,
    Event3 = command_checklistbox_toggled,
    Event4 = command_listbox_selected,

    %% Listen to events from GUI
    Dict2  = connectMainFrame(Frame, Dict),
    Dict3  = connectMsgFrame(Frame,  Dict2),
    Dict4  = connectDlg(Login,    "loginOk",              Dict3),
    Dict5  = connectDlg(Login,    "loginCancel",          Dict4),
    Dict6  = connectDlg(Time,     "reminderOk",           Dict5),
    Dict7  = connectDlg(Time,     "reminderCancel",       Dict6),
    Dict8  = connectDlg(Time,     "useReminder",          Event1, Dict7),
    Dict9  = connectDlg(Time,     "startDate",            Event2, Dict8),
    Dict10 = connectDlg(Search,   "searchOk",             Dict9),
    Dict11 = connectDlg(Search,   "searchCancel",         Dict10),
    Dict12 = connectDlg(Users,    "userOk",               Dict11),
    Dict13 = connectDlg(Users,    "userCancel",           Dict12),
    Dict14 = connectDlg(AddList,  "listOk",               Dict13),
    Dict15 = connectDlg(AddList,  "listCancel",           Dict14),
    Dict16 = connectDlg(AddList,  "listCheckListBox",     Event3, Dict15),
    Dict17 = connectDlg(Settings, "settingsOk",           Dict16),
    Dict18 = connectDlg(Settings, "settingsCancel",       Dict17),
    Dict19 = connectDlg(Settings, "webUIEnabled",         Event1, Dict18),
    Dict20 = connectDlg(ManLists, "manageListOk",         Dict19),
    Dict21 = connectDlg(ManLists, "manageListCancel",     Dict20),
    Dict22 = connectDlg(ManLists, "createListButton",     Dict21),
    Dict23 = connectDlg(ManLists, "removeListButton",     Dict22),
    Dict24 = connectDlg(ManLists, "updateListButton",     Dict23),
    Dict25 = connectDlg(ManLists, "manageListBox",        Event4, Dict24),
    Dict26 = connectDlg(ManOwner, "manageOwnerOk",        Dict25),
    Dict27 = connectDlg(ManOwner, "manageOwnerCancel",    Dict26),
    Dict28 = connectDlg(ManOwner, "createOwnerButton",    Dict27),
    Dict29 = connectDlg(ManOwner, "removeOwnerButton",    Dict28),
    Dict30 = connectDlg(ManOwner, "updateOwnerButton",    Dict29),
    Dict31 = connectDlg(ManOwner, "manageOwnerBox",       Event4, Dict30),
    Dict32 = connectDlg(ManBookm, "manageBookmarkOk",     Dict31),
    Dict33 = connectDlg(ManBookm, "manageBookmarkCancel", Dict32),
    Dict34 = connectDlg(ManBookm, "createBookmarkButton", Dict33),
    Dict35 = connectDlg(ManBookm, "removeBookmarkButton", Dict34),
    Dict36 = connectDlg(ManBookm, "updateBookmarkButton", Dict35),
    Dict37 = connectDlg(ManBookm, "manageBookmarkBox",    Event4, Dict36),
    Dict38 = connectDlg(SortCols, "sortColumnsOk",        Dict37),
    Dict39 = connectDlg(SortCols, "sortColumnsCancel",    Dict38),
    Dict40 = connectDlg(SortCols, "moveColUpButton",      Dict39),
    Dict41 = connectDlg(SortCols, "moveColDownButton",    Dict40),
    Dict42 = connectDlg(SortCols, "sortColumnsBox",       Event4, Dict41),
    Dict43 = connectDlg(Timer,    "timerOk",              Dict42),
    Dict44 = connectDlg(Timer,    "timerCancel",          Dict43),
    Dict45 = connectDlg(Plugins,  "pluginOk",             Dict44),
    Dict46 = connectDlg(Plugins,  "pluginCancel",         Dict45),
    Dict47 = connectDlg(Plugins,  "usePlugins",           Event4, Dict46),
    Dict48 = connectDlg(LogWork,  "logWorkOk",            Dict47),
    Dict49 = connectDlg(LogWork,  "logWorkCancel",        Dict48),
    Dict50 = connectDlg(LogWork,  "workDate",             Event2, Dict49),


    connectModalDialog(Conflict, "useLocalButton",  ?useLocal),
    connectModalDialog(Conflict, "useRemoteButton", ?useRemote),
    connectModalDialog(About,    "aboutButton",     ?about),

    Print = wxHtmlEasyPrinting:new(),

    State = #guiState{dict        = Dict50,
                      rows        = eRows:new(),
                      startup     = Arg,
                      user        = DefUser,
                      print       = Print,
                      frame       = Frame,
                      msgMenu     = MsgMenu,
                      loginDlg    = Login,
                      timeDlg     = Time,
                      timerDlg    = Timer,
                      searchDlg   = Search,
                      pluginDlg   = Plugins,
                      logWorkDlg  = LogWork,
                      usersDlg    = Users,
                      addListDlg  = AddList,
                      manListsDlg = ManLists,
                      manOwnerDlg = ManOwner,
                      manBookmDlg = ManBookm,
                      sortColsDlg = SortCols,
                      settingsDlg = Settings,
                      conflictDlg = Conflict,
                      aboutDlg    = About,
                      toolBar     = wxFrame:getToolBar(Frame),
                      menuBar     = wxFrame:getMenuBar(Frame)},

    StatusBar = obj("mainStatusBar", State),
    wxStatusBar:setStatusText(StatusBar, DefUser,        [{number, 0}]),
    wxStatusBar:setStatusText(StatusBar, ?sbNotLoggedIn, [{number, 1}]),

    TodoList = getTodoList(?defTaskList, State),
    wxListCtrl:assignImageList(TodoList, IL, ?wxIMAGE_LIST_SMALL),

    Columns  = eTodoDB:getColumns(DefUser),
    [wxListCtrl:insertColumn(TodoList, Col, Desc) || {Col, Desc} <- Columns],

    %% Set size on main window
    UserConfig = setWindowSize(DefUser, Frame),

    State2 = State#guiState{filter    = UserConfig#userCfg.filter},
    State3 = State2#guiState{bookmCfg = default(UserConfig#userCfg.bookmCfg, [])},

    TodoLists = getTodoLists(DefUser),
    setTaskLists(TodoLists, State3),

    State4 = updateTodoWindow(State3),

    ePluginServer:setConfiguredPlugins(default(UserConfig#userCfg.plugins, [])),

    %% Update column size
    [setColumnWidth(TodoList, DefUser, Col, Desc) || {Col, Desc} <- Columns],

    State5 =
        case getETodoAtIndex(0, State3#guiState.rows) of
            undefined ->
                State4;
            ETodo ->
                updateGui(ETodo, 0, State4)
        end,

    %% Set where help is shown.
    wxFrame:setStatusBarPane(Frame, 3),

    %% Disable backTool
    Back    = xrcId("backTool"),
    ToolBar = wxFrame:getToolBar(Frame),
    wxToolBar:enableTool(ToolBar, Back, false),

    wxFrame:show(Frame),
    %% Set size on splitterWindows.
    setSplitterPos(UserConfig, Frame),
    case DefUserCfg#userCfg.showLogin of
        true ->
            eGuiEvents:loginMenuItemEvent(undefined,
                                          undefined,
                                          undefined,  State4);
        false ->
            ok
    end,

    %% Set initial date for work log report.
    WorkLoadObj  = obj("workLogStartDate", State),
    LastWeekDays = calendar:date_to_gregorian_days(date()) - 4,
    LastWeek     = calendar:gregorian_days_to_date(LastWeekDays),
    wxDatePickerCtrl:setValue(WorkLoadObj, {LastWeek, {0,0,0}}),

    State6 = eGuiFunctions:generateWorkLog(State5),
    State7 = eGuiFunctions:generateTimeLog(State6),
    State8 = eGuiFunctions:generateSchedule(State7),

    {Frame, checkStatus(State8#guiState{columns = Columns})}.

setWindowSize(DefUser, Frame) ->
    UserCfg = eTodoDB:readUserCfg(DefUser),
    case UserCfg#userCfg.windowSize of
        {X, Y} ->
            wxFrame:setSize(Frame, {X, Y});
        _ ->
            ok
    end,
    UserCfg.

setSplitterPos(UserCfg, Frame) ->
    %% Set size main splitter
    SplitterMain = obj("splitterMain", Frame),

    case UserCfg#userCfg.splitterMain of
        MainPos when is_integer(MainPos) ->
            wxSplitterWindow:setSashPosition(SplitterMain, MainPos);
        _ ->
            ok
    end,

    %% Set size comment splitter
    SplitterComment = obj("splitterComment", Frame),

    case UserCfg#userCfg.splitterComment of
        CommentPos when is_integer(CommentPos) ->
            wxSplitterWindow:setSashPosition(SplitterComment, CommentPos);
        _ ->
            ok
    end,

    %% Set size comment splitter
    SplitterMsg = obj("splitterMsg", Frame),

    case UserCfg#userCfg.splitterMsg of
        MsgPos when is_integer(MsgPos) ->
            wxSplitterWindow:setSashPosition(SplitterMsg, MsgPos);
        _ ->
            ok
    end.

saveGuiText(State = #guiState{activeTodo = {undefined, _}}) ->
    State;
saveGuiText(State = #guiState{activeTodo = {ETodo, Index},
                              user       = User}) ->
    case getGuiText(State) of
        ETodo ->
            State;
        ETodo2 ->
            updateTodoInDB(User, ETodo2),
            TaskList = getTaskList(State),
            TodoList = getTodoList(TaskList, State),
            State2   = updateTodo(TodoList, ETodo2, Index, State),
            eLog:log(debug, ?MODULE, saveGuiText, [ETodo2],
                     "Active todo updated.", ?LINE),
            checkUndoStatus(State2#guiState{activeTodo = {ETodo2, Index}})
    end.

saveGuiSettings(command_text_updated, State) ->
    saveGuiText(State);
saveGuiSettings(_Type, State) ->
    saveGuiSettings(State).

saveGuiSettings(State = #guiState{activeTodo = {undefined, _}}) ->
    State;
saveGuiSettings(State = #guiState{activeTodo = {ETodo, Index}}) ->
    case getGuiSettings(State) of
        ETodo ->
            State;
        ETodo2->
            setDoneTimeStamp(ETodo2, Index, State)
    end.

getGuiText(State = #guiState{activeTodo = {ETodo, _}}) ->
    DescObj     = obj("descriptionArea", State),
    CommentObj  = obj("commentArea",     State),
    Descript    = wxTextCtrl:getValue(DescObj),
    Comment     = wxTextCtrl:getValue(CommentObj),
    ETodo#etodo{description  = Descript,
                comment      = Comment}.

getGuiSettings(State = #guiState{activeTodo = {ETodo, _}}) ->
    SharedObj       = obj("sharedWithText",  State),
    StatusObj       = obj("statusChoice",    State),
    PriorityObj     = obj("priorityChoice",  State),
    DueDateObj      = obj("dueDatePicker",   State),
    DueDateUsedObj  = obj("dueDateUsed",     State),
    ProgressObj     = obj("progressInfo",    State),
    OwnerObj        = obj("ownerChoice",     State),
    SelNum1         = wxChoice:getSelection(StatusObj),
    SelAtom1        = toDB(wxChoice:getString(StatusObj, SelNum1)),
    SelNum2         = wxChoice:getSelection(PriorityObj),
    SelAtom2        = toDB(wxChoice:getString(PriorityObj, SelNum2)),
    SharedTxt       = wxStaticText:getLabel(SharedObj),
    SharedWith      = string:tokens(SharedTxt, ";"),
    DueTime         = case wxCheckBox:isChecked(DueDateUsedObj) of
                          true ->
                              wxDatePickerCtrl:getValue(DueDateObj);
                          false ->
                              undefined
                      end,
    Progress        = wxSpinCtrl:getValue(ProgressObj),
    SelOwner        = wxChoice:getSelection(OwnerObj),
    Owner           = wxChoice:getString(OwnerObj, SelOwner),
    ETodo#etodo{statusDB     = SelAtom1,
                status       = toStr(SelAtom1),
                priorityDB   = SelAtom2,
                priority     = toStr(SelAtom2),
                dueTimeDB    = wxDate2Date(DueTime),
                dueTime      = toStr(wxDate2Date(DueTime)),
                sharedWithDB = SharedWith,
                sharedWith   = SharedTxt,
                progress     = Progress,
                owner        = Owner}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(getSearchCfg, _From, State = #guiState{searchCfg = Cfg}) ->
    {reply, Cfg, State};
handle_call({getFilterCfg, TaskList}, _From, State = #guiState{filter = Cfg}) ->
    Filter = useFilter(TaskList, Cfg, State),
    {reply, Filter, State};
handle_call(getTimeReportData, _From, State) ->
    TaskList = getTaskList(State),
    AllTask  = TaskList == ?defTaskList,
    Reply    = {ok, State#guiState.rows, AllTask},
    {reply, Reply, State};
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
handle_cast({todoUpdated, _Sender, Todo = #todo{}},
            State = #guiState{mode = noGui,
                              user = User}) ->
    NewTodo = eTodoDB:newTodo(Todo),
    eTodoDB:updateTodoNoLocks(Todo),
    case NewTodo of
        true ->
            Row = eTodoDB:getRow(User, ?defTaskList),
            eTodoDB:addTodo(#userInfo{userName = User,
                                      uid      = Todo#todo.uid,
                                      row      = Row,
                                      parent   = ?defTaskList}, Todo);
        false ->
            ok
    end,
    {noreply, State};
handle_cast({todoUpdated, _Sender, Todo = #diff{}},
            State = #guiState{mode = noGui}) ->
    case eTodoDB:newTodo(Todo) of
        true ->
            %% Shouldn't be a diff, this is a removed eTodo, ignore.
            ok;
        false ->
            eTodoDB:updateTodoNoLocks(Todo)
    end,
    {noreply, State};
handle_cast({loggedIn, Peer}, State = #guiState{mode = noGui,
                                                user = User}) ->
    case {os:getenv("eTodoStatus"), os:getenv("eTodoStatusMsg")} of
        {Status, StatusMsg} when
              (Status =/= false) and (StatusMsg =/= false) ->
            StatusUpdate = #userStatus{userName  = User,
                                       status    = Status,
                                       statusMsg = StatusMsg},
            eWeb:setStatusUpdate(User, Status, StatusMsg),
            ePeerEM:sendMsg(User, [Peer], statusEntry,
                            {statusUpdate, StatusUpdate, getPortrait(User)});
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({todoDeleted, Uid},
            State = #guiState{user = User, mode = noGui}) ->
    %% Remove task from database.
    eTodoDB:delTodo(Uid, User),
    {noreply, State};
handle_cast({taskListDeleted, List},
    State = #guiState{user = User, mode = noGui}) ->
    TodoLists1 = getTodoLists(User),
    TodoLists2 = lists:delete(List, TodoLists1),
    eTodoDB:moveTodosToTaskList(User, ?defTaskList, List),
    UserCfg = eTodoDB:readUserCfg(User),
    eTodoDB:saveUserCfg(UserCfg#userCfg{lists = TodoLists2}),
    {noreply, State};
handle_cast(_Msg, State = #guiState{mode = noGui}) ->
    {noreply, State};
handle_cast({acceptingIncCon, User, Circle, Port}, State) ->
    StatusBarObj = obj("mainStatusBar", State),
    PortStr = integer_to_list(Port),
    ConCfg  = default(eTodoDB:getConnection(User), #conCfg{host = "localhost"}),
    Host    = default(ConCfg#conCfg.host, "localhost"),
    ConnectionInfo = Circle ++ "(" ++ Host ++ ":" ++ PortStr ++ ")",
    wxStatusBar:setStatusText(StatusBarObj, User,   [{number, 0}]),
    wxStatusBar:setStatusText(StatusBarObj, ConnectionInfo, [{number, 1}]),
    {noreply, State};
handle_cast({msgEntry, User, Users, Text},
            State = #guiState{frame = Frame,
                              user  = UserName}) ->
    MsgObj = obj("msgTextWin", State),
    appendToPage(MsgObj, eHtml:generateMsg(UserName, User, Users, Text)),
    Msg = "Received message from " ++ User,
    State2 = chatMsgStatusBar(Msg, State),
    raiseIfIconified(Frame),
    ReplyAll = lists:delete(UserName, Users),
    saveMsg(UserName, User, Users, Text, State),
    {noreply, State2#guiState{reply = User, replyAll = ReplyAll}};
handle_cast({systemEntry, system, Text}, State) ->
    MsgObj       = obj("msgTextWin",    State),
    appendToPage(MsgObj, eHtml:generateSystemMsg(system, Text)),
    State2 = sysMsgStatusBar("System message received.", State),
    {noreply, State2};
handle_cast({systemEntry, Uid, Text}, State) ->
    MsgObj       = obj("msgTextWin",    State),
    appendToPage(MsgObj, eHtml:generateSystemMsg(Uid, Text)),
    sysMsgStatusBar("System message received.", State),
    {noreply, State};
handle_cast({alarmEntry, Uid, Text}, State) ->
    MsgObj       = obj("msgTextWin",    State),
    RemObj       = obj("remTextWin",    State),
    appendToPage(MsgObj, eHtml:generateAlarmMsg(Uid, Text)),
    appendToReminders(RemObj, eHtml:generateAlarmMsg(Uid, Text)),

    sysMsgStatusBar("Alarm message received.", State),
    {noreply, State};
handle_cast({loggedIn, User}, State = #guiState{userStatus = Users}) ->
    UserObj      = obj("userCheckBox",  State),
    StatusBarObj = obj("mainStatusBar", State),
    Opt = [{number, 2}],
    wxStatusBar:setStatusText(StatusBarObj, User ++ " logged in.", Opt),
    wxCheckListBox:append(UserObj, User),
    Users2     = lists:keydelete(User, #userStatus.userName, Users),
    UserStatus = #userStatus{userName = User},
    State2 = userStatusUpdate(State#guiState{userStatus = [UserStatus|Users2]}),
    {noreply, State2};
handle_cast({loggedOut, User}, State = #guiState{userStatus= Users}) ->
    UserObj = obj("userCheckBox",  State),
    Count   = wxCheckListBox:getCount(UserObj),
    State2  =
        case Count > 0 of
            true ->
                case wxCheckListBox:findString(UserObj, User) of
                    Index when Index >= 0 ->
                        wxCheckListBox:delete(UserObj, Index);
                    _ ->
                        ok
                end,
                Users2 = lists:keydelete(User, #userStatus.userName, Users),
                setPeerStatusIfNeeded(State#guiState{userStatus = Users2});
            false ->
                State
        end,
    {noreply, State2};
handle_cast(taskListsUpdated, State = #guiState{user = User}) ->
    TodoLists = getTodoLists(User),
    setTaskLists(TodoLists, State),
    {noreply, State};
handle_cast({taskListDeleted, List}, State = #guiState{user = User}) ->
    TodoLists1 = getTodoLists(User),
    TodoLists2 = lists:delete(List, TodoLists1),
    setTaskLists(TodoLists2, State),
    {noreply, State};
handle_cast({todoCreated, TaskList, _DBRow, Todo},
            State = #guiState{user       = User,
                              columns    = Columns}) ->
    State2 = case getTaskList(State) of
                 TaskList ->
                     ETodo    = makeETodo(Todo, User, Columns),
                     TodoList = getTodoList(TaskList, State),
                     Row = wxListCtrl:getItemCount(TodoList),
                     addTodo(TodoList, ETodo, Row, State);
                 _ ->
                     State
             end,
    {noreply, State2};
handle_cast({todoDeleted, Uid},
            State = #guiState{user = User, rows = Rows}) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    State4 =
        case findIndex(Uid, Rows) of
            {Index, _} ->
                wxListCtrl:deleteItem(TodoList, Index),

                %% Remove from internal data structure.
                Rows2  = eRows:deleteRow(Uid, State#guiState.rows),
                State2 = State#guiState{rows = Rows2},

                %% Remove task from database.
                eTodoDB:delTodo(Uid, User),

                %% Select correct task
                State3 = deleteAndUpdate(Index, TodoList, State2),

                focusAndSelect(Index, State3#guiState{activeTodo = {undefined, -1}});
            _ ->
                %% Remove task from database.
                eTodoDB:delTodo(Uid, User),
                State
        end,
    {noreply, State4};
handle_cast({writing, Sender}, State) ->
    StatusBarObj = obj("mainStatusBar", State),
    Opt = [{number, 2}],
    wxStatusBar:setStatusText(StatusBarObj, Sender ++ " writing...", Opt),
    {noreply, State};
handle_cast({statusUpdate, UserStatus, Avatar},
            State = #guiState{userStatus = Users}) ->
    Users2 = lists:keyreplace(UserStatus#userStatus.userName,
                              #userStatus.userName, Users, UserStatus),
    %% Update status bar with status info.
    Msg =  UserStatus#userStatus.userName ++
        " (" ++ UserStatus#userStatus.status ++ ")",
    StatusBarObj = obj("mainStatusBar", State),
    wxStatusBar:setStatusText(StatusBarObj, Msg, [{number, 3}]),
    (catch saveAvatar(UserStatus#userStatus.userName, Avatar)),
    State2 = setPeerStatusIfNeeded(State#guiState{userStatus = Users2}),
    {noreply, State2};
handle_cast({todoUpdated, _Sender, Todo = #todo{}},
            State = #guiState{user       = User,
                              rows       = Rows,
                              columns    = Columns,
                              activeTodo = {_ETodo, ActiveIndex}}) ->
    NewTodo  = eTodoDB:newTodo(Todo),
    eTodoDB:updateTodoNoLocks(Todo),
    ETodo    = makeETodo(Todo, User, Columns),
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    State3 = case findIndex(Todo#todo.uid, Rows) of
                 {ActiveIndex, OldETodo} ->
                     State2 = updateTodo(TodoList, ETodo, ActiveIndex, State),
                     updateGui(OldETodo, ETodo, ActiveIndex, State2);
                 {Index, _} ->
                     updateTodo(TodoList, ETodo, Index, State);
                 _ ->
                     createNewTodo(NewTodo, User, Todo, ETodo, State)
             end,
    {noreply, State3};
handle_cast({todoUpdated, _Sender, Todo = #diff{}},
            State = #guiState{user       = User,
                              rows       = Rows,
                              columns    = Columns,
                              activeTodo = {_ETodo, ActiveIndex}}) ->
    State3 =
        case eTodoDB:newTodo(Todo) of
            true ->
                %% Shouldn't be a diff, this is a removed task, ignore.
                State;
            false ->
                eTodoDB:updateTodoNoLocks(Todo),
                Todo2    = eTodoDB:getTodo(Todo#diff.uid),
                ETodo    = makeETodo(Todo2, User, Columns),
                TaskList = getTaskList(State),
                TodoList = getTodoList(TaskList, State),
                case findIndex(Todo2#todo.uid, Rows) of
                    {ActiveIndex, OldETodo} ->
                        State2= updateTodo(TodoList, ETodo, ActiveIndex, State),
                        updateGui(OldETodo, ETodo, ActiveIndex, State2);
                    {Index, _} ->
                        updateTodo(TodoList, ETodo, Index, State);
                    _ ->
                        %% Task should exist, ignore
                        State
                end
        end,
    {noreply, State3};
handle_cast({checkConflict, User, PeerUser,
             OldLocalConTime, OldRemoteConTime, Diff}, State) ->
    State2 = doCheckConflict(User, PeerUser,
                             OldLocalConTime, OldRemoteConTime,
                             Diff, State),
    {noreply, State2};
handle_cast({delayedUpdateGui, ETodo, Index}, State) ->
    %% Do not execute old update, because a new eTodo has already been selected.
    cancelTimer(State#guiState.delayedUpdate),
    Ref = erlang:send_after(100, self(), {delayedUpdateGui, ETodo, Index}),
    {noreply, State#guiState{delayedUpdate = Ref}};
handle_cast({launchBrowser, URL}, State) ->
    wx_misc:launchDefaultBrowser(URL),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_event(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_event(#wx{id = Id, event = #wxCommand{type = Type}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxContextMenu{type = Type}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxSpin{type = Type}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxNotebook{type = Type}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxMouse{type = Type}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxList{type = Type,
                                          col  = Col}},
             State = #guiState{frame = Frame, dict  = Dict})
  when Type == command_list_col_right_click ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Col, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxList{type      = Type,
                                          itemIndex = Row}},
             State = #guiState{frame = Frame, dict  = Dict})
  when Type == command_list_item_right_click ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Row, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxList{type      = Type,
                                          code      = Code}},
             State = #guiState{frame = Frame, dict  = Dict})
  when Type == command_list_key_down ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Code, Id, Frame, State);

handle_event(#wx{id = Id, event = #wxList{type      = Type,
                                          itemIndex = Index}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Index, Id, Frame, State);

handle_event(#wx{event = #wxClose{}, obj = Frame},
             State = #guiState{frame = Frame}) ->
    State2 = saveGuiSettings(State),
    catch wxWindows:'Destroy'(Frame),
    {stop, normal, State2};

handle_event(#wx{event = #wxDate{}, obj = Obj},
             State = #guiState{timeDlg = Obj}) ->
    State2 = eGuiEvents:startDateChangedEvent(State),
    State3 = saveGuiSettings(State2),
    checkStatus(State3),
    {noreply, State3};

handle_event(#wx{id = Id, event = #wxDate{type = Type}},
             State = #guiState{frame = Frame, dict  = Dict}) ->
    Name = getFromDict(Id, Dict),
    handle_cmd(Name, Type, Id, Frame, State);

handle_event(#wx{event = #wxHtmlLink{linkInfo = LinkInfo}}, State) ->
    State2 = saveGuiSettings(State),
    State4 =
        case catch convertUid(LinkInfo#wxHtmlLinkInfo.href) of
            {'EXIT', _Reason} ->
                wx_misc:launchDefaultBrowser(LinkInfo#wxHtmlLinkInfo.href),
                State2;
            {uid, Uid} ->
                %% ETodo link clicked, select correct task in task list.
                case eTodoDB:getTodo(Uid) of
                    undefined ->
                        State2;
                    _ ->
                        showTodo(Uid, State2)
                end;
            {uidAndDate, Uid, Date} ->
                case eTodoDB:getTodo(Uid) of
                    undefined ->
                        State2;
                    _ ->
                        showLogWork(Uid, Date, State2)
                end
        end,
    {noreply, State4};

handle_event(Event = #wx{}, State) ->
    eLog:log(debug, ?MODULE, handle_event, [Event],
             "Unhandled event received.", ?LINE),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({delayedUpdateGui, ETodo, Index}, State) ->
    State2 = updateGui(ETodo, Index, State),
    {noreply, State2#guiState{delayedUpdate = undefined}};
handle_info({timerEnded, MsgTxt}, State = #guiState{user = User}) ->
    alarmEntry(timer, MsgTxt),
    ePluginServer:eReceivedAlarmMsg("Notify: " ++ MsgTxt),
    ePluginServer:eTimerEnded(User, MsgTxt),
    {noreply, State#guiState{timerRef = undefined}};
handle_info({timerEnded, MsgTxt, Command}, State = #guiState{user = User}) ->
    alarmEntry(timer, MsgTxt),
    ePluginServer:eTimerEnded(User, MsgTxt),
    %% Run command for Pomodoro timer.
    os:cmd(Command),
    {noreply, State#guiState{timerRef = undefined}};
handle_info(DownMsg = {'DOWN', _Reference, process, _Object, _Info},
            State = #guiState{user = User}) ->
    eLog:log(error, ?MODULE, handle_info, [DownMsg],
             "Recieved down message from eWeb.", ?LINE),
    {ok, Pid} = eWeb:start_link(User),
    monitor(process, Pid),
    {noreply, State};
handle_info({setPomodoroClock, _Obj1, _Obj2, _Obj3, _Obj4},
            State = #guiState{timerDlgOpen = false}) ->
    {noreply, State};
handle_info(Msg = {setPomodoroClock, Obj1, Obj2, Obj3, Obj4}, State) ->
    case wxCheckBox:isChecked(Obj4) of
        false ->
            ok;
        true ->
            setTeamPomodoroClock(Obj1, Obj2, Obj3)
    end,
    erlang:send_after(1000, self(), Msg),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #guiState{mode = noGui}) ->
    init:stop(),
    ok;
terminate(_Reason, State = #guiState{startup = normal}) ->
    saveColumnSizes(State),
    saveEtodoSettings(State),
    doLogout(State#guiState.user, State),
    wx:destroy(),
    ok;
terminate(_Reason, State) ->
    saveColumnSizes(State),
    saveEtodoSettings(State),
    doLogout(State#guiState.user, State),
    wx:destroy(),
    init:stop(),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
setIcon(Frame) ->
    Png    = case smallIcon() of
                 true ->
                     wxImage:new("./Icons/etodoSmall.png");
                 false ->
                     wxImage:new("./Icons/etodoBig.png")
             end,
    Bitmap = wxBitmap:new(Png),
    Icon   = wxIcon:new(),
    wxIcon:copyFromBitmap(Icon, Bitmap),
    wxFrame:setIcon(Frame, Icon).

smallIcon() ->
    case os:type() of
        {unix, _} ->
            false;
        {win32, nt} ->
            false;
        _ ->
            true
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
handle_cmd(Name, Type, Id, Frame, State) ->
    Function = list_to_atom(Name ++ "Event"),
    case catch eGuiEvents:Function(Type, Id, Frame, State) of
        {'EXIT', Reason} ->
            eLog:log(debug, ?MODULE, handle_cmd, [Reason],
                     "No callback implemented.", ?LINE),
            {noreply, State};
        stop ->
            {stop, normal, State};
        State2 ->
            State3 = saveGuiSettings(Type, State2),
            {noreply, State3}
    end.

handle_cmd(Name, Type, Index, Id, Frame, State) ->
    Function = list_to_atom(Name ++ "Event"),
    case catch eGuiEvents:Function(Type, Index, Id, Frame, State) of
        {'EXIT', Reason} ->
            eLog:log(debug, ?MODULE, handle_cmd, [Reason],
                     "No callback implemented.", ?LINE),
            {noreply, State};
        stop ->
            {stop, normal, State};
        State2 ->
            {noreply, State2}
    end.
%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
connectMsgFrame(Frame, Dict) ->
    AllMsgObj = wxXmlResource:xrcctrl(Frame, "msgTextWin", wxHtmlWindow),
    RemMsgObj = wxXmlResource:xrcctrl(Frame, "remTextWin", wxHtmlWindow),
    wxHtmlWindow:connect(AllMsgObj, right_down),
    wxHtmlWindow:connect(RemMsgObj, right_down),
    Dict2  = connectItems(["msgTextCtrl"],
                          [command_text_enter, command_text_updated],
                          Frame,   Dict),
    connectItems(["msgTextWin", "remTextWin", "workLogReport",
                  "timeLogReport", "scheduleReport",
                  "descAreaPreview", "commentAreaPreview"],
                 command_html_link_clicked, Frame, Dict2).

connectMainFrame(Frame, Dict) ->
    ok = wxFrame:connect(Frame, close_window),

    ToolList = ["todoUpTool", "todoDownTool", "moveUpTool", "moveDownTool",
                "newTasklistTool", "cutTool", "copyTool", "pasteTool",
                "deleteTool", "undoTool", "redoTool", "searchTool",
                "backTool", "forwardTool", "timerTool"],

    MenuItems = ["loginMenuItem", "logoutMenuItem", "exitMenuItem",
                 "cutMenu", "copyMenu", "pasteMenu", "deleteMenu",
                 "undoMenu", "redoMenu", "aboutMenu", "linkViewMenu",
                 "linkTimeReportMenu", "addTaskInboxMenu",
                 "linkFileMenu", "settingsMenu", "printMenuItem",
                 "moveFirstMenu", "moveLastMenu", "proxyLinkMenu",
                 "moveUpMenu", "moveDownMenu", "addTaskMenu", "helpMenu1",
                 "replyMenu", "replyAllMenu", "setAvatarMenuItem",
                 "backupMenuItem", "restoreMenuItem", "pluginMenuItem"],

    MenuChoice = ["userStatusChoice", "statusChoice", "priorityChoice",
                  "taskListChoice", "ownerChoice"],

    TextAreas = ["descriptionArea", "commentArea"],

    ListEvents = [command_list_col_right_click,
                  command_list_item_right_click,
                  command_list_item_activated,
                  command_list_item_selected,
                  command_list_key_down],

    Buttons = ["setReminderButton", "commentButton",
               "manageListsButton", "sendTaskButton","addOwnerButton",
               "configureSearch", "shareButton", "addListButton",
               "bookmarkBtn", "logWorkButton"],

    TextFields = ["searchText", "userStatusMsg"],

    Dict2  = connectItems(ToolList, command_menu_selected,      Frame, Dict),
    Dict3  = connectItems(["mainTaskList"], ListEvents,         Frame, Dict2),
    Dict4  = connectItems(MenuChoice, command_choice_selected,  Frame, Dict3),
    Dict5  = connectItems(["dueDatePicker"], date_changed,      Frame, Dict4),
    Dict6  = connectItems(["dueDateUsed"],
                          command_checkbox_clicked,             Frame, Dict5),
    Dict7  = connectItems(TextAreas, command_text_updated,      Frame, Dict6),
    Dict8  = connectItems(Buttons, command_button_clicked,      Frame, Dict7),
    Dict9  = connectItems(TextFields, command_text_enter,       Frame, Dict8),
    Dict10 = connectItems(["checkBoxUseFilter"],
                          command_checkbox_clicked,             Frame, Dict9),
    Dict11 = connectItems(["mainNotebook",
                           "descNotebook",
                           "commentNotebook"],
                          command_notebook_page_changed,        Frame, Dict10),
    Dict12 = connectItems(["userCheckBox"],
                          command_listbox_selected,             Frame, Dict11),
    Dict13 = connectItems(["progressInfo"],
                          command_spinctrl_updated,             Frame, Dict12),
    Dict14 = connectItems(["bookmarkBtn"], context_menu,        Frame, Dict13),
    Dict15 = connectItems(["workLogStartDate"], date_changed,   Frame, Dict14),
    connectItems(MenuItems, command_menu_selected,              Frame, Dict15).

connectItems(_Items, [], _Frame, Dict) ->
    Dict;
connectItems([], _Event, _Frame, Dict) ->
    Dict;
connectItems(Items, [Event|Rest], Frame, Dict) ->
    Dict2 = connectItems(Items, Event, Frame, Dict),
    connectItems(Items, Rest, Frame, Dict2);
connectItems([Item | Rest], Event, Frame, Dict) ->
    Dict2 = connectItem(Frame, Item, Event, Dict),
    connectItems(Rest, Event, Frame, Dict2).

connectItem(Frame, Name, Event, Dict)
  when (Name == "mainTaskList") and (Event == command_list_key_down) ->
    Id = xrcId(Name),
    wxFrame:connect(Frame, Event, [{id, Id}, {skip, true}]),
    Dict#{Id => Name};
connectItem(Frame, Name, Event, Dict) ->
    Id = xrcId(Name),
    wxFrame:connect(Frame, Event, [{id, Id}]),
    Dict#{Id => Name}.

connectDlg(Dialog, Name, Dict) ->
    Dict2 = connectDlg(Dialog, Name, command_button_clicked, Dict),
    connectDlg(Dialog, Name, command_enter, Dict2).

connectDlg(Dialog, Name, Event, Dict) ->
    Id = xrcId(Name),
    wxDialog:connect(Dialog, Event, [{id, Id}]),
    Dict#{Id => Name}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
connectModalDialog(Dialog, ButtonName, ButtonId) ->
    wxDialog:connect(Dialog, command_button_clicked,
                     [{id, xrcId(ButtonName)},
                      {callback, fun(_, _) ->
                                         wxDialog:endModal(Dialog, ButtonId)
                                 end}]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getFromDict(Key, Dict) ->
    maps:get(Key, Dict, "gui").

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
setUserNameAndFocus(Login, DefUserCfg) ->
    User2 = case DefUserCfg#userCfg.lastUser of
                undefined ->
                    wx_misc:getUserId();
                User ->
                    User
            end,
    LoginField  = wxXmlResource:xrcctrl(Login, "userName",  wxTextCtrl),
    PwdField    = wxXmlResource:xrcctrl(Login, "circlePwd", wxTextCtrl),
    wxTextCtrl:setValue(LoginField, User2),
    wxTextCtrl:setFocus(PwdField), % hej
    UserCfg = eTodoDB:readUserCfg(default),
    eTodoDB:saveUserCfg(UserCfg#userCfg{lastUser = User2}),
    %% Logged in to client not to circle, tell alarm server to start
    %% checking for alarms.
    eTodoAlarm:loggedIn(User2),
    User2.


saveEtodoSettings(#guiState{frame = Frame, user = User, filter = Filter}) ->
    Size            = wxFrame:getSize(Frame),
    SplitterMain    = obj("splitterMain", Frame),
    MainPos         = wxSplitterWindow:getSashPosition(SplitterMain),
    SplitterComment = obj("splitterComment", Frame),
    CommentPos      = wxSplitterWindow:getSashPosition(SplitterComment),
    SplitterMsg     = obj("splitterMsg", Frame),
    MsgPos          = wxSplitterWindow:getSashPosition(SplitterMsg),
    Plugins         = ePluginServer:getConfiguredPlugins(),
    UserCfg         = eTodoDB:readUserCfg(User),

    eTodoDB:saveUserCfg(UserCfg#userCfg{windowSize      = Size,
                                        splitterMain    = MainPos,
                                        splitterComment = CommentPos,
                                        splitterMsg     = MsgPos,
                                        filter          = Filter,
                                        plugins         = Plugins}).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
createNewTodo(true, User, Todo, ETodo, State) ->
    Row = eTodoDB:getRow(User, ?defTaskList),
    eTodoDB:addTodo(#userInfo{userName = User,
                              uid      = Todo#todo.uid,
                              row      = Row,
                              parent   = ?defTaskList}, Todo),
    TodoList = getTodoList(?defTaskList, State),
    systemEntry(Todo#todo.uid, "New: " ++ Todo#todo.description),
    addTodo(TodoList, ETodo, Row, State);
createNewTodo(false, _User, _Todo, _ETodo, State) ->
    State.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
fillConflictWindow(Local, Remote, State = #guiState{user    = User,
                                                    columns = Columns}) ->
    eLog:log(debug, ?MODULE, fillConflictWindow, [Local, Remote],
             "Handle conflict.", ?LINE),
    ETodoLocal  = makeETodo(Local,  User, Columns),
    ETodoRemote = makeETodo(Remote, User, Columns),
    fillRemoteConflict(ETodoRemote, State),
    fillLocalConflict(ETodoLocal,   State),
    State.

fillRemoteConflict(ETodo, State = #guiState{conflictDlg = CD}) ->
    DescObj     = obj("descriptionArea1", CD),
    CommentObj  = obj("commentArea1",     CD),
    SharedObj   = obj("sharedWithText1",  CD),
    StatusObj   = obj("statusChoice1",    CD),
    PrioObj     = obj("priorityChoice1",  CD),
    DueDateObj  = obj("dueDatePicker1",   CD),
    ProgressObj = obj("progressInfo1",    CD),
    OwnerObj    = obj("ownerChoice1",     CD),
    setOwner(OwnerObj, State#guiState.user, ETodo#etodo.owner),
    wxTextCtrl:setValue(DescObj, ETodo#etodo.description),
    wxTextCtrl:setValue(CommentObj, ETodo#etodo.comment),
    setSelection(StatusObj, ETodo#etodo.status),
    setSelection(PrioObj, ETodo#etodo.priority),
    wxTextCtrl:setValue(SharedObj, ETodo#etodo.sharedWith),
    wxDatePickerCtrl:setValue(DueDateObj, date2wxDate(ETodo#etodo.dueTimeDB)),
    wxSpinCtrl:setValue(ProgressObj, default(ETodo#etodo.progress, 0)).

fillLocalConflict(ETodo, State = #guiState{conflictDlg = CD}) ->
    DescObj     = obj("descriptionArea2", CD),
    CommentObj  = obj("commentArea2",     CD),
    SharedObj   = obj("sharedWithText2",  CD),
    StatusObj   = obj("statusChoice2",    CD),
    PrioObj     = obj("priorityChoice2",  CD),
    DueDateObj  = obj("dueDatePicker2",   CD),
    ProgressObj = obj("progressInfo2",    CD),
    OwnerObj    = obj("ownerChoice2",     CD),
    setOwner(OwnerObj, State#guiState.user, ETodo#etodo.owner),
    wxTextCtrl:setValue(DescObj, ETodo#etodo.description),
    wxTextCtrl:setValue(CommentObj, ETodo#etodo.comment),
    setSelection(StatusObj, ETodo#etodo.status),
    setSelection(PrioObj, ETodo#etodo.priority),
    wxTextCtrl:setValue(SharedObj, ETodo#etodo.sharedWith),
    wxDatePickerCtrl:setValue(DueDateObj, date2wxDate(ETodo#etodo.dueTimeDB)),
    wxSpinCtrl:setValue(ProgressObj, default(ETodo#etodo.progress, 0)).

getConflictSettings(Todo, #guiState{conflictDlg = CD}) ->
    DescObj     = obj("descriptionArea2", CD),
    CommentObj  = obj("commentArea2",     CD),
    SharedObj   = obj("sharedWithText2",  CD),
    StatusObj   = obj("statusChoice2",    CD),
    PriorityObj = obj("priorityChoice2",  CD),
    DueDateObj  = obj("dueDatePicker2",   CD),
    ProgressObj = obj("progressInfo2",    CD),
    OwnerObj    = obj("ownerChoice2",     CD),
    Descript    = wxTextCtrl:getValue(DescObj),
    Comment     = wxTextCtrl:getValue(CommentObj),
    SelNum1     = wxChoice:getSelection(StatusObj),
    SelAtom1    = toDB(wxChoice:getString(StatusObj, SelNum1)),
    SelNum2     = wxChoice:getSelection(PriorityObj),
    SelAtom2    = toDB(wxChoice:getString(PriorityObj, SelNum2)),
    SharedTxt   = wxTextCtrl:getValue(SharedObj),
    SharedWith  = string:tokens(SharedTxt, ";"),
    DueTime     = wxDatePickerCtrl:getValue(DueDateObj),
    Progress    = wxSpinCtrl:getValue(ProgressObj),
    SelOwner    = wxChoice:getSelection(OwnerObj),
    Owner       = wxChoice:getString(OwnerObj, SelOwner),
    Todo#todo{description = Descript,
              comment     = Comment,
              status      = SelAtom1,
              priority    = SelAtom2,
              sharedWith  = SharedWith,
              dueTime     = wxDate2Date(DueTime),
              progress    = Progress,
              owner       = Owner}.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
doCheckConflict(_User, _PeerUser, _OldLocalConTime, _OldRemoteConTime,
                {[], []}, State) ->
    State;
doCheckConflict(User, PeerUser, OldLocalConTime, OldRemoteConTime,
                {[], [Remote|Rest]}, State) ->
    case Remote#todo.createTime > OldRemoteConTime of
        true ->
            %% Only remote add to local.
            todoUpdated(PeerUser, Remote);
        false ->
            %% We have already removed this one.
            eLog:log(debug, ?MODULE, checkConflict, [Remote],
                     "Already removed todo, do not update local todo.", ?LINE)
    end,
    doCheckConflict(User, PeerUser,
                    OldLocalConTime, OldRemoteConTime,
                    {[], Rest}, State);
doCheckConflict(User, PeerUser,
                OldLocalConTime, OldRemoteConTime,
                {[Local|Rest], []}, State) ->
    %% Only local, send to other peer if this one is new since last connect.
    case Local#todo.createTime > OldLocalConTime of
        true ->
            ePeerEM:todoUpdated(User, [PeerUser], Local);
        false ->
            eLog:log(debug, ?MODULE, checkConflict, [Local],
                     "Already removed todo, do not update todo on "
                     "other peers.", ?LINE)
    end,
    doCheckConflict(User, PeerUser,
                    OldLocalConTime, OldRemoteConTime,
                    {Rest, []}, State);
doCheckConflict(User, PeerUser,
                OldLocalConTime, OldRemoteConTime, {[Local|Rest], RemoteList},
                State = #guiState{conflictDlg = Conflict}) ->
    LocalTodo = eTodoDB:getTodo(Local#todo.uid), %% Get newest data
    case lists:keytake(Local#todo.uid, #todo.uid, RemoteList) of
        {value, LocalTodo, RemoteRest} -> %% They are the same
            doCheckConflict(User, PeerUser,
                            OldLocalConTime, OldRemoteConTime,
                            {Rest, RemoteRest}, State);
        {value, Remote, RemoteRest} ->    %% They differ
            State2 = fillConflictWindow(Local, Remote, State),
            case wxDialog:showModal(Conflict) of
                ?useLocal ->
                    Local2 = getConflictSettings(Local, State),
                    eTodoDB:updateTodoNoDiff(User, Local2);
                ?useRemote ->
                    eTodoDB:updateTodoNoDiff(User, Remote);
                ?wxID_CANCEL ->
                    eTodoDB:updateTodoNoDiff(User, Local)
            end,
            State3 = updateTodoWindow(State2),
            doCheckConflict(User, PeerUser,
                            OldLocalConTime, OldRemoteConTime,
                            {Rest, RemoteRest}, State3);
        _ ->
            %% Only local, send to other peer if this
            %% one is new since last connect.
            case Local#todo.createTime > OldLocalConTime of
                true ->
                    ePeerEM:todoUpdated(User, [PeerUser], Local);
                false ->
                    eLog:log(debug, ?MODULE, checkConflict, [Local],
                             "Already removed todo, do not update "
                             "todo on other peers.", ?LINE)
            end,
            doCheckConflict(User, PeerUser,
                            OldLocalConTime, OldRemoteConTime,
                            {Rest, RemoteList}, State)
    end.

%%======================================================================
%% Function : raiseIfIconified(Frame) -> ok
%% Purpose  : Make icon move/show activity.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
raiseIfIconified(Frame) ->
    case wxFrame:isIconized(Frame) of
        true ->
            wxHtmlWindow:raise(Frame);
        false ->
            ok
    end.

%%======================================================================
%% Function : chatMsgStatusBar(Msg, State) -> ok
%% Purpose  : Add message to status bar if "Messages" isn't active.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
chatMsgStatusBar(Msg, State) ->
    Notebook = obj("mainNotebook",  State),
    CurrPage = wxNotebook:getCurrentPage(Notebook),
    MsgPage  = wxNotebook:getPage(Notebook, 1),
    case CurrPage of
        MsgPage ->
            clearStatusBar(State);
        _ ->
            State2 = increaseMsgCounter(State),
            StatusBarObj = obj("mainStatusBar", State2),
            wxStatusBar:setStatusText(StatusBarObj, Msg, [{number, 2}]),
            State2
    end.

sysMsgStatusBar(Msg, State) ->
    StatusBarObj = obj("mainStatusBar", State),
    Notebook     = obj("mainNotebook",  State),
    CurrPage     = wxNotebook:getCurrentPage(Notebook),
    TaskPage     = wxNotebook:getPage(Notebook, 0),
    case CurrPage of
        TaskPage ->
            State2 = increaseSysMsgCounter(Msg, State),
            wxStatusBar:setStatusText(StatusBarObj, Msg, [{number, 2}]),
            State2;
        _ ->
            State
    end.

increaseMsgCounter(State = #guiState{unreadMsgs = Before}) ->
    Num = Before + 1,
    Notebook = obj("mainNotebook",  State),
    wxNotebook:setPageText(Notebook, 1,
                           "Messages(" ++ integer_to_list(Num) ++ ")"),
    State#guiState{unreadMsgs = Num}.

%% Only increase counter for alarms.
increaseSysMsgCounter("Alarm message received.",
                      State = #guiState{unreadSysMsgs = Before}) ->
    Num = Before + 1,
    Notebook = obj("mainNotebook",  State),
    wxNotebook:setPageText(Notebook, 2,
                           "Reminders(" ++ integer_to_list(Num) ++ ")"),
    State#guiState{unreadSysMsgs = Num};
increaseSysMsgCounter(_Msg, State) ->
    increaseMsgCounter(State).

%%====================================================================
%% Event to trigger gui save
%%====================================================================
showTodo(Uid, State = #guiState{frame = Frame, columns = Columns, user = User}) ->
    %% Show window when clicking on ETodo link.
    wxFrame:raise(Frame),

    Notebook = obj("mainNotebook",  State),
    wxNotebook:changeSelection(Notebook, 0),

    %% Look in current list
    case findIndex(Uid, State#guiState.rows) of
        {Index, _ETodo} when Index >= 0 ->
            focusAndSelect(Index, State);
        _ ->  %% Not in current list, add it temporarily.
            TaskList  = getTaskList(State),
            TodoList  = getTodoList(TaskList, State),
            Row       = wxListCtrl:getItemCount(TodoList),
            Todo      = eTodoDB:getTodo(Uid),

            ETodo     = makeETodo(Todo, User, Columns),
            State2    = addTodo(TodoList, ETodo, Row, State),
            State3    = updateGui(ETodo, Row, State2),
            focusAndSelect(Row, State3)
    end.

%%====================================================================
%% Save custom avatar to disk.
%%====================================================================
saveAvatar(_Peer, undefined) ->
    ok;
saveAvatar(Peer, Icon) ->
    CustomPortrait1 = getRootDir() ++ "/Icons/portrait_" ++ Peer ++ ".png",
    CustomPortrait2 = getRootDir() ++ "/www/priv/Icons/portrait_" ++ Peer ++ ".png",
    file:write_file(CustomPortrait1, Icon),
    file:write_file(CustomPortrait2, Icon).

%%====================================================================
%% Show log work dialog.
%%====================================================================
showLogWork(Uid, Date, State) ->
    %% ETodo link clicked, open log work dialog.
    Todo    = eTodoDB:getTodo(Uid),
    User    = State#guiState.user,
    Columns = State#guiState.columns,
    ETodo   = makeETodo(Todo, User, Columns),
    LWDlg   = State#guiState.logWorkDlg,
    State3  = State#guiState{activeTodo = {ETodo, -1}},
    DateObj = wxXmlResource:xrcctrl(LWDlg, "workDate", wxDatePickerCtrl),
    wxDatePickerCtrl:setValue(DateObj, date2wxDate(Date)),
    eGuiEvents:logWorkButtonEvent(undefined, undefined,
                                  undefined, State3),
    State3.


%%====================================================================
%% Set team pomodoro clock
%%====================================================================
setTeamPomodoroClock(Obj1, Obj2, Obj3) ->
    {Minutes, Seconds} = getTeamPomodoroClock(),
    wxSpinCtrl:setValue(Obj1, 0),
    wxSpinCtrl:setValue(Obj2, Minutes),
    wxSpinCtrl:setValue(Obj3, Seconds).

getTeamPomodoroClock() ->
    {_Hours, Min, Seconds} = time(),
    case Min of
        Min when (Min >= 25) and (Min < 30) ->
            {0, 0};
        Min when Min >= 55 ->
            {0, 0};
        Min when Min < 25 ->
            {24 - Min, 59 - Seconds};
        _ ->
            {54 - Min, 59 - Seconds}
    end.

