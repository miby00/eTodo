%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 4 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eGuiEvents).
-author("mikael.bylund@gmail.com").

-include_lib("eTodo/include/eTodo.hrl").

-include_lib("wx/include/wx.hrl").

%% API
-export([aboutMenuEvent/4,
         addListButtonEvent/4,
         addBookmarkButtonEvent/4,
         addOwnerButtonEvent/4,
         addTaskMenuEvent/4,
         addTaskInboxMenuEvent/4,
         backToolEvent/4,
         backupMenuItemEvent/4,
         bookmarkBtnEvent/4,
         checkBoxUseFilterEvent/4,
         commentAreaEvent/4,
         commentButtonEvent/4,
         commentNotebookEvent/4,
         configureSearchEvent/4,
         copyMenuEvent/4,
         copyToolEvent/4,
         createBookmarkButtonEvent/4,
         createListButtonEvent/4,
         createOwnerButtonEvent/4,
         cutMenuEvent/4,
         cutToolEvent/4,
         deleteMenuEvent/4,
         deleteToolEvent/4,
         descNotebookEvent/4,
         descriptionAreaEvent/4,
         dueDatePickerEvent/4,
         dueDateUsedEvent/4,
         exitMenuItemEvent/4,
         forwardToolEvent/4,
         guiEvent/4,
         helpMenu1Event/4,
         linkFileMenuEvent/4,
         linkViewMenuEvent/4,
         linkTimeReportMenuEvent/4,
         proxyLinkMenuEvent/4,
         listCancelEvent/4,
         listCheckListBoxEvent/4,
         listOkEvent/4,
         loginCancelEvent/4,
         loginMenuItemEvent/4,
         loginOkEvent/4,
         logoutMenuItemEvent/4,
         logWorkButtonEvent/4,
         logWorkCancelEvent/4,
         logWorkOkEvent/4,
         mainNotebookEvent/4,
         mainTaskListEvent/5,
         manageBookmarkBoxEvent/4,
         manageBookmarkCancelEvent/4,
         manageBookmarkOkEvent/4,
         manageListBoxEvent/4,
         manageListCancelEvent/4,
         manageListOkEvent/4,
         manageListsButtonEvent/4,
         manageOwnerBoxEvent/4,
         manageOwnerCancelEvent/4,
         manageOwnerOkEvent/4,
         moveColUpButtonEvent/4,
         moveColDownButtonEvent/4,
         moveDownMenuEvent/4,
         moveLastMenuEvent/4,
         moveDownToolEvent/4,
         moveFirstMenuEvent/4,
         moveUpMenuEvent/4,
         moveUpToolEvent/4,
         msgNotebookEvent/4,
         msgTextCtrlEvent/4,
         msgTextWinEvent/4,
         msgFilterEvent/4,
         msgSettingsButtonEvent/4,
         msgSettingsCancelEvent/4,
         msgSettingsOkEvent/4,
         ownerChoiceEvent/4,
         pasteMenuEvent/4,
         pasteToolEvent/4,
         pluginCancelEvent/4,
         pluginOkEvent/4,
         pluginMenuItemEvent/4,
         printMenuItemEvent/4,
         priorityChoiceEvent/4,
         progressInfoEvent/4,
         redoMenuEvent/4,
         redoToolEvent/4,
         reminderCancelEvent/4,
         reminderOkEvent/4,
         removeBookmarkButtonEvent/4,
         removeListButtonEvent/4,
         removeOwnerButtonEvent/4,
         restoreMenuItemEvent/4,
         replyAllMenuEvent/4,
         sendChatMenuEvent/4,
         replyMenuEvent/4,
         searchCancelEvent/4,
         searchOkEvent/4,
         searchTextEvent/4,
         sendChatMsgEvent/4,
         sendTaskButtonEvent/4,
         setAvatarMenuItemEvent/4,
         setReminderButtonEvent/4,
         settingsCancelEvent/4,
         settingsMenuEvent/4,
         settingsOkEvent/4,
         shareButtonEvent/4,
         startDateChangedEvent/1,
         statusChoiceEvent/4,
         sortColumnsBoxEvent/4,
         sortColumnsCancelEvent/4,
         sortColumnsOkEvent/4,
         smileyHappyEvent/4,
         smileyMischiefEvent/4,
         smileyLOLEvent/4,
         smileyShockedEvent/4,
         smileyCryingEvent/4,
         smileyWinkEvent/4,
         smileyHeartEvent/4,
         linkFileButtonEvent/4,
         timerOkEvent/4,
         timerCancelEvent/4,
         timerToolEvent/4,
         taskListChoiceEvent/4,
         todoDownToolEvent/4,
         todoUpToolEvent/4,
         undoMenuEvent/4,
         undoToolEvent/4,
         updateBookmarkButtonEvent/4,
         updateListButtonEvent/4,
         updateOwnerButtonEvent/4,
         usePluginsEvent/4,
         useReminderEvent/4,
         userCancelEvent/4,
         userCheckBoxEvent/4,
         userOkEvent/4,
         userStatusChoiceEvent/4,
         userStatusMsgEvent/4,
         webUIEnabledEvent/4,
         webProxyEnabledEvent/4,
         smtpEnabledEvent/4,
         workDateEvent/4,
         workLogReportEvent/4,
         workLogStartDateEvent/4]).

-import(eGuiFunctions, [addTodo/4,
                        appendToPage/4,
                        appendToPage/6,
                        checkStatus/1,
                        checkUndoStatus/1,
                        clearAndInitiate/2,
                        clearMsgCounter/1,
                        clearStatusBar/1,
                        deleteAndUpdate/3,
                        doLogout/2,
                        focusAndSelect/1,
                        focusAndSelect/2,
                        getCheckedItems/1,
                        getTaskList/1,
                        getTodoList/2,
                        getTodoLists/1,
                        getWorkDesc/2,
                        makeETodo/3,
                        obj/2,
                        pos/2,
                        saveColumnSizes/1,
                        setUnreadMsgs/2,
                        setColumnWidth/4,
                        setColor/2,
                        setOwner/3,
                        setPeerStatusIfNeeded/1,
                        setPortrait/2,
                        setScrollBar/1,
                        setSelection/1,
                        setSelection/2,
                        setTaskLists/2,
                        showBookmarkMenu/2,
                        showMenu/4,
                        toClipboard/2,
                        updateGui/3,
                        updateGui/4,
                        updateTodo/4,
                        updateMsgWindow/2,
                        updateTodoInDB/2,
                        updateTodoWindow/1,
                        useFilter/3,
                        userStatusAvailable/1,
                        userStatusUpdate/1,
                        wxDate2Date/1,
                        date2wxDate/1,
                        xrcId/1]).

-import(eTodoUtils, [cancelTimer/1,
                     default/2,
                     getRootDir/0,
                     makeRef/0,
                     makeStr/1,
                     taskExternal/1,
                     taskInternal/1,
                     toStatusDB/1,
                     toStr/1,
                     tryInt/1,
                     getWeekDay/1]).

-import(eRows, [findIndex/2,
                getETodoAtIndex/2,
                swapRows/3,
                updateRows/2]).

%%====================================================================
%% Component callbacks, don't forget to add them to export list
%%====================================================================

%%====================================================================
%% Menu item exit
%%====================================================================
exitMenuItemEvent(_Type, _Id, _Frame, _State) ->
    stop.

%%====================================================================
%% Menu item logout
%%====================================================================
logoutMenuItemEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    doLogout(User, State).

%%====================================================================
%% Menu item login
%%====================================================================
loginMenuItemEvent(_Type, _Id, _Frame,  State = #guiState{loginDlg = Login}) ->
    UserObj        = wxXmlResource:xrcctrl(Login, "userName",     wxTextCtrl),
    CircleObj      = wxXmlResource:xrcctrl(Login, "circleName",   wxTextCtrl),
    PasswordObj    = wxXmlResource:xrcctrl(Login, "circlePwd",    wxTextCtrl),
    ShowLoginObj   = wxXmlResource:xrcctrl(Login, "showLogin",    wxCheckBox),
    LoginCancelObj = wxXmlResource:xrcctrl(Login, "loginCancel",  wxButton),

    %% Move show login checkbox after cancel button in tab order.
    wxWindow:moveAfterInTabOrder(ShowLoginObj, LoginCancelObj),

    #userCfg{lastUser   = LastUser,
             lastCircle = LastCircle,
             showLogin  = ShowLogin} = eTodoDB:readUserCfg(default),

    wxTextCtrl:setValue(UserObj,   default(LastUser,   "")),
    wxTextCtrl:setValue(CircleObj, default(LastCircle, "eTodo")),
    wxTextCtrl:setValue(PasswordObj, ""),
    wxTextCtrl:setFocus(PasswordObj),
    wxCheckBox:setValue(ShowLoginObj, default(ShowLogin, false)),
    wxTextCtrl:setValue(PasswordObj, ""),

    %% Make sure login dialog is on top.
    wxDialog:show(Login),
    wxDialog:raise(Login),
    wxDialog:setFocus(Login),
    wxTextCtrl:setFocus(PasswordObj),
    State.

loginOkEvent(Type, Id, Frame,  State = #guiState{loginDlg = Login,
                                                 user   = OldUser}) ->
    UserObj      = wxXmlResource:xrcctrl(Login, "userName",   wxTextCtrl),
    CircleObj    = wxXmlResource:xrcctrl(Login, "circleName", wxTextCtrl),
    PasswordObj  = wxXmlResource:xrcctrl(Login, "circlePwd",  wxTextCtrl),
    User         = wxTextCtrl:getValue(UserObj),
    Circle       = wxTextCtrl:getValue(CircleObj),
    Password     = wxTextCtrl:getValue(PasswordObj),
    Default      = eTodoDB:readUserCfg(default),
    wxDialog:hide(Login),
    Md5Pwd = crypto:hash(md5, Password),
    case checkPassword(Frame, Default, User, Circle, Md5Pwd) of
        true ->
            loginToCircle(Default, OldUser,
                          User, Circle, Password, Md5Pwd, State);
        false ->
            loginMenuItemEvent(Type, Id, Frame, State)
    end.

checkPassword(Frame, UserCfg, User, Circle, Password) ->
    case UserCfg#userCfg.oldPwd of
        {User, Circle, Password} ->
            %% Same password as last time.
            true;
        {User, Circle, _Pwd} ->
            %% Warn user, that login was done with a new password
            checkIfLogin(Frame);
        _ ->
            %% New Username or circle, password should be diffrent
            true
    end.

checkIfLogin(Frame) ->
    MsgDlg = wxMessageDialog:new(Frame, "Login to circle using new password?",
                                 [{caption, "New password"},
                                  {style, ?wxYES_NO}]),
    case wxDialog:showModal(MsgDlg) of
        ?wxID_YES ->
            wxDialog:destroy(MsgDlg),
            true;
        ?wxID_NO ->
            wxDialog:destroy(MsgDlg),
            false
    end.

loginToCircle(Default, OldUser, User, Circle, Password, Md5Pwd,
              State = #guiState{loginDlg = Login,
                                loggedIn = LoggedIn,
                                frame    = Frame}) ->
    ShowLoginObj = wxXmlResource:xrcctrl(Login, "showLogin",  wxCheckBox),
    ShowLogin    = wxCheckBox:isChecked(ShowLoginObj),
    OldPwd       = {User, Circle, Md5Pwd},
    eTodoDB:saveUserCfg(Default#userCfg{lastUser   = User,
                                        lastCircle = Circle,
                                        showLogin  = ShowLogin,
                                        oldPwd     = OldPwd}),
    wxFrame:setTitle(Frame, "eTodo - " ++ User ++ " (" ++ Circle ++ ")"),
    (catch doLogout(OldUser, LoggedIn)),
    if LoggedIn -> timer:sleep(4000);
       true     -> ok
    end,
    TodoLists = getTodoLists(User),
    State2 = State#guiState{user = User},
    setTaskLists(TodoLists, State2),
    %% Add handler to receive events
    ePeerEM:del_handler(eTodoEH, User),
    ePeerEM:add_handler(eTodoEH, User),
    ePeerEM:connectToCircle(User, Circle, Password),
    eTodoAlarm:loggedIn(User),
    UserCfg = eTodoDB:readUserCfg(User),
    ePluginServer:setConfiguredPlugins(default(UserCfg#userCfg.plugins, [])),
    %% Start web gui
    eWeb:start_link(User),
    State3 = updateTodoWindow(State2),
    State4 = userStatusAvailable(State3),
    State5 = setUnreadMsgs(User, State4),
    State6 = updateMsgWindow(State5, User),
    focusAndSelect(State6#guiState{loggedIn = true}).

loginCancelEvent(_Type, _Id, _Frame,  State = #guiState{loginDlg = Login}) ->
    wxDialog:hide(Login),
    State.

%%====================================================================
%% Backup and restore database
%%====================================================================

backupMenuItemEvent(_Type, _Id, Frame,  State) ->
    FileSaveDlg = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE}]),
    wxFileDialog:setFilename(FileSaveDlg, "eTodoDB.bup"),
    case wxFileDialog:showModal(FileSaveDlg) of
        ?wxID_OK ->
            FullPath = wxFileDialog:getPath(FileSaveDlg),
            mnesia:backup(FullPath);
        ?wxID_CANCEL ->
            ok
    end,
    wxFileDialog:destroy(FileSaveDlg),
    State.
restoreMenuItemEvent(_Type, _Id, Frame,  State) ->
    FileOpenDlg = wxFileDialog:new(Frame, [{style, ?wxFD_OPEN}]),
    case wxFileDialog:showModal(FileOpenDlg) of
        ?wxID_OK ->
            FullPath = wxFileDialog:getPath(FileOpenDlg),
            mnesia:restore(FullPath, [{default_op, recreate_tables}]);
        ?wxID_CANCEL ->
            ok
    end,
    wxFileDialog:destroy(FileOpenDlg),
    State2 = updateTodoWindow(State),
    eTodoDB:clearUndo(),
    checkUndoStatus(State2).

%%====================================================================
%% Reminder button
%%====================================================================
setReminderButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{activeTodo = {undefined, _}}) ->
    eLog:log(debug, ?MODULE, handle_event, [State],
             "Active todo undefined.", ?LINE),
    State;
setReminderButtonEvent(Type, Id, Frame,
                       State = #guiState{timeDlg    = Time,
                                         user       = User,
                                         activeTodo = {ETodo, _Index}}) ->
    case eTodoDB:getReminder(User, ETodo#etodo.uidDB) of
        #alarmCfg{startDate  = StartDate,
                  startTime  = StartTime,
                  recurrence = Recurrence,
                  execCmd    = Cmd,
                  endDate    = EndDate} ->
            setReminderData(StartDate, StartTime,
                            EndDate, Cmd, Recurrence, State);
        _ ->
            setReminderData(undefined, undefined,
                            undefined, undefined, undefined, State)
    end,
    State2 = useReminderEvent(Type, Id, Frame, State),
    startDateChangedEvent(State2),
    wxDialog:show(Time),
    State2.

dlgObj(Name, Type, #guiState{timeDlg = Time}) ->
    wxXmlResource:xrcctrl(Time, Name, Type).

setReminderData(StartDate, StartTime, EndDate, Cmd, Recurrence, State) ->
    StartDateObj    = dlgObj("startDate",    wxDatePickerCtrl, State),
    EndDateObj      = dlgObj("endDate",      wxDatePickerCtrl, State),
    RecurrenceObj   = dlgObj("recurrence",   wxRadioBox,       State),
    UseReminderObj  = dlgObj("useReminder",  wxCheckBox,       State),
    UseStartTimeObj = dlgObj("useStartTime", wxCheckBox,       State),
    ExecuteCmdObj   = dlgObj("executeCmd",   wxCheckBox,       State),
    ExecCmdObj      = dlgObj("execCmd",      wxTextCtrl,       State),
    HourObj         = dlgObj("hourSpin",     wxSpinCtrl,       State),
    MinObj          = dlgObj("minSpin",      wxSpinCtrl,       State),
    UseEndDateObj   = dlgObj("useEndDate",   wxCheckBox,       State),
    setCmd(Cmd, ExecuteCmdObj, ExecCmdObj),
    setDate(StartDate, StartDateObj, UseReminderObj),
    setDate(EndDate,   EndDateObj,   UseEndDateObj),
    setTime(StartTime, HourObj, MinObj, UseStartTimeObj),
    setRecurrence(Recurrence, RecurrenceObj).

setCmd(undefined, ExecuteCmdObj, _ExecCmdObj) ->
    wxCheckBox:setValue(ExecuteCmdObj, false);
setCmd(Cmd, ExecuteCmdObj, ExecCmdObj) ->
    wxCheckBox:setValue(ExecuteCmdObj, true),
    wxTextCtrl:setValue(ExecCmdObj, Cmd).

setTime(undefined, HourObj, MinObj, UseStartTimeObj) ->
    wxCheckBox:setValue(UseStartTimeObj, false),
    wxSpinCtrl:setValue(HourObj, 0),
    wxSpinCtrl:setValue(MinObj,  0);
setTime({HH, MM}, HourObj, MinObj, UseStartTimeObj) ->
    wxCheckBox:setValue(UseStartTimeObj, true),
    wxSpinCtrl:setValue(HourObj, HH),
    wxSpinCtrl:setValue(MinObj,  MM).

getTime(CheckObj, HourObj, MinObj) ->
    case wxCheckBox:isChecked(CheckObj) of
        true ->
            HH = wxSpinCtrl:getValue(HourObj),
            MM = wxSpinCtrl:getValue(MinObj),
            {HH, MM};
        false ->
            undefined
    end.

setDate(undefined, DateObj, Use)  ->
    wxCheckBox:setValue(Use, false),
    wxDatePickerCtrl:setValue(DateObj, date2wxDate(undefined));
setDate(Date, DateObj, Use) ->
    wxCheckBox:setValue(Use, true),
    wxDatePickerCtrl:setValue(DateObj, date2wxDate(Date)).

getDate(CheckObj, DateObj) ->
    case wxCheckBox:isChecked(CheckObj) of
        true ->
            wxDate2Date(wxDatePickerCtrl:getValue(DateObj));
        false ->
            undefined
    end.

setRecurrence(undefined,  Obj)   ->
    wxRadioBox:setSelection(Obj, 0);
setRecurrence(Recurrence, Obj) ->
    Values = [none, time15, time1h, time2h, time4h,
              timeDay, timeWeek, timeMonth, timeYear],
    wxRadioBox:setSelection(Obj, pos(Recurrence, Values)).

getRecurrence(State) ->
    Obj    = dlgObj("recurrence",  wxRadioBox, State),
    Result = wxRadioBox:getSelection(Obj),
    Values = {none, time15, time1h, time2h, time4h,
              timeDay, timeWeek, timeMonth, timeYear},
    element(Result + 1, Values).

reminderOkEvent(_Type, _Id, _Frame,
                State = #guiState{timeDlg    = Time,
                                  user       = User,
                                  activeTodo = {ETodo, _Index}}) ->
    StartDateObj    = dlgObj("startDate",    wxDatePickerCtrl, State),
    EndDateObj      = dlgObj("endDate",      wxDatePickerCtrl, State),
    UseReminderObj  = dlgObj("useReminder",  wxCheckBox,       State),
    UseStartTimeObj = dlgObj("useStartTime", wxCheckBox,       State),
    UseEndDateObj   = dlgObj("useEndDate",   wxCheckBox,       State),
    HourObj         = dlgObj("hourSpin",     wxSpinCtrl,       State),
    MinObj          = dlgObj("minSpin",      wxSpinCtrl,       State),
    StartDate       = getDate(UseReminderObj, StartDateObj),
    EndDate         = getDate(UseEndDateObj,  EndDateObj),
    StartTime       = getTime(UseStartTimeObj, HourObj, MinObj),
    Recurrence      = getRecurrence(State),
    eTodoDB:addReminder(#alarmCfg{uid        = ETodo#etodo.uidDB,
                                  userName   = User,
                                  startDate  = StartDate,
                                  startTime  = StartTime,
                                  endDate    = EndDate,
                                  execCmd    = getCmd(State),
                                  recurrence = Recurrence}),
    wxDialog:hide(Time),
    State.

getCmd(State) ->
    ExecuteCmdObj = dlgObj("executeCmd", wxCheckBox, State),
    ExecCmdObj    = dlgObj("execCmd",    wxTextCtrl, State),
    case wxCheckBox:isChecked(ExecuteCmdObj) of
        true ->
            wxTextCtrl:getValue(ExecCmdObj);
        false ->
            undefined
    end.

reminderCancelEvent(_Type, _Id, _Frame,  State = #guiState{timeDlg = Time}) ->
    wxDialog:hide(Time),
    State.

useReminderEvent(_Type, _Id, _Frame, State) ->
    StartDateObj    = dlgObj("startDate",    wxDatePickerCtrl, State),
    EndDateObj      = dlgObj("endDate",      wxDatePickerCtrl, State),
    UseReminderObj  = dlgObj("useReminder",  wxCheckBox,       State),
    UseEndDateObj   = dlgObj("useEndDate",   wxCheckBox,       State),
    UseStartTimeObj = dlgObj("useStartTime", wxCheckBox,       State),
    ExecuteCmdObj   = dlgObj("executeCmd",   wxCheckBox,       State),
    ExecCmdObj      = dlgObj("execCmd",      wxTextCtrl,       State),
    HourObj         = dlgObj("hourSpin",     wxSpinCtrl,       State),
    MinObj          = dlgObj("minSpin",      wxSpinCtrl,       State),
    RecurrenceObj   = dlgObj("recurrence",   wxRadioBox,       State),
    OKObj           = dlgObj("reminderOk",   wxButton,         State),
    case wxCheckBox:isChecked(UseReminderObj) of
        true ->
            wxDatePickerCtrl:enable(StartDateObj),
            wxDatePickerCtrl:enable(EndDateObj),
            wxCheckBox:enable(UseEndDateObj),
            wxCheckBox:enable(UseStartTimeObj),
            wxCheckBox:enable(ExecuteCmdObj),
            wxTextCtrl:enable(ExecCmdObj),
            wxSpinCtrl:enable(HourObj),
            wxSpinCtrl:enable(MinObj),
            wxRadioBox:enable(RecurrenceObj);
        false ->
            wxDatePickerCtrl:disable(StartDateObj),
            wxDatePickerCtrl:disable(EndDateObj),
            wxCheckBox:disable(UseEndDateObj),
            wxCheckBox:disable(ExecuteCmdObj),
            wxTextCtrl:disable(ExecCmdObj),
            wxCheckBox:disable(UseStartTimeObj),
            wxSpinCtrl:disable(HourObj),
            wxSpinCtrl:disable(MinObj),
            wxRadioBox:disable(RecurrenceObj),
            wxButton:enable(OKObj)
    end,
    State.

startDateChangedEvent(State) ->
    StartDateObj = dlgObj("startDate",  wxDatePickerCtrl, State),
    OKObj        = dlgObj("reminderOk", wxButton,         State),
    case wxDate2Date(wxDatePickerCtrl:getValue(StartDateObj)) of
        undefined ->
            wxButton:disable(OKObj);
        _ ->
            wxButton:enable(OKObj)
    end,
    State.

%%====================================================================
%% Toolbar show/hide msg window.
%%====================================================================
msgTextCtrlEvent(Event = #wxKey{}, Id, Frame, State) ->
    MsgTextCtrl = obj("msgTextCtrl",  State),
    Empty       = wxTextCtrl:getValue(MsgTextCtrl) == "",

    case {Empty, Event} of
        {false, #wxKey{keyCode     = 13,
                       controlDown = false,
                       altDown     = false,
                       metaDown    = false,
                       shiftDown   = false}} ->
            sendChatMenuEvent(Event, Id, Frame, State),
            State;
        _ ->
            State
    end;
msgTextCtrlEvent(command_text_updated, _Id, _Frame,
                 State = #guiState{user          = User,
                                   msgStatusSent = SentStatus,
                                   menuBar       = MenuBar}) ->
    MsgTextCtrl = obj("msgTextCtrl",  State),
    SMChatObj   = xrcId("sendChatMenu"),

    Empty       = wxTextCtrl:getValue(MsgTextCtrl) == "",
    if
        Empty ->
            wxMenuBar:enable(MenuBar, SMChatObj, false),
            wxBitmapButton:disable(obj("sendChatMsg", State));
        true ->
            wxMenuBar:enable(MenuBar, SMChatObj, true),
            wxBitmapButton:enable(obj("sendChatMsg",  State))
    end,
    case getCheckedItems(obj("userCheckBox", State)) of
        [] ->
            State;
        Users ->
            case {SentStatus, Empty} of
                {false, false} ->
                    ePeerEM:sendMsg(User, Users, statusEntry, writing),
                    State#guiState{msgStatusSent = true};
                _ ->
                    State
            end
    end;
msgTextCtrlEvent(kill_focus, _Id, _Frame,
                 State = #guiState{menuBar = MenuBar}) ->
    SMChatObj = xrcId("sendChatMenu"),
    wxMenuBar:enable(MenuBar, SMChatObj, false),
    State;
msgTextCtrlEvent(set_focus, _Id, _Frame,
                 State = #guiState{menuBar = MenuBar}) ->
    MsgTextCtrl = obj("msgTextCtrl",  State),
    SMChatObj   = xrcId("sendChatMenu"),
    NotEmpty    = wxTextCtrl:getValue(MsgTextCtrl) =/= "",
    wxMenuBar:enable(MenuBar, SMChatObj, NotEmpty),
    State.


sendChatMsgEvent(_Event, _Id, _Frame, State) ->
    UserObj = obj("userCheckBox", State),
    MsgObj  = obj("msgTextWin",   State),
    Users   = getCheckedItems(UserObj),
    case getCheckedItems(UserObj) of
        [] ->
            MsgDlg = wxMessageDialog:new(MsgObj, "Message has no recipient",
                                         [{caption, "Send message failed"}]),
            wxDialog:showModal(MsgDlg),
            wxDialog:destroy(MsgDlg),
            State;
        Users ->
            sendMsg(Users, State)
    end.

sendMsg(Users, State = #guiState{user = User}) ->
    MsgTextCtrl = obj("msgTextCtrl",  State),
    MsgObj      = obj("msgTextWin",   State),
    MsgText     = wxTextCtrl:getValue(MsgTextCtrl),

    wxTextCtrl:clear(MsgTextCtrl),
    appendToPage(MsgObj, msgEntry, User, Users,
                 eHtml:generateMsg(User, User, Users, MsgText), State),
    ePeerEM:sendMsg(User, Users, msgEntry, MsgText),
    State#guiState{msgStatusSent = false}.

%%====================================================================
%% Show right click menu on message window.
%%====================================================================
msgTextWinEvent(_Type, _Id, Frame, State = #guiState{msgMenu = MsgMenu}) ->
    wxWindow:popupMenu(Frame, MsgMenu),
    State.

workLogReportEvent(_Type, _Id, Frame, State = #guiState{user = User}) ->
    showMenu(User, {row, -1}, Frame, State).

%%====================================================================
%%  Task list event (command_list_item_selected  or
%%                   command_list_item_activated or
%%                   command_list_item_right_click)
%%====================================================================
mainTaskListEvent(command_list_key_down, Code, Id, Frame, State) ->
    case Code of
        314 ->
            backToolEvent(command_list_key_down, Id, Frame, State);
        316 ->
            case hasSubTodo(State) of
                true ->
                    drillDown(State);
                false ->
                    State
            end;
        _ ->
            State
    end;
mainTaskListEvent(command_list_item_selected, Index, _Id, _Frame,
                  State = #guiState{activeTodo = {ActiveTodo, Index}}) ->
    ETodo = getETodoAtIndex(Index, State#guiState.rows),
    case ETodo of
        ActiveTodo ->
            %% Same task has been selected.
            checkStatus(State);
        ETodo ->
            %% A new task has been selected, update GUI.
            updateGui(ETodo, Index, State)
    end;
mainTaskListEvent(command_list_item_selected, Index, _Id, _Frame, State) ->
    ETodo = getETodoAtIndex(Index, State#guiState.rows),
    case State#guiState.activeTodo of
        {ETodo, _}  ->
            %% This happens when we move a task up and down the list.
            checkStatus(State);
        _ ->
            %% A new task has been selected, update GUI.
            eTodo:delayedUpdateGui(ETodo, Index),
            State
    end;
mainTaskListEvent(command_list_item_activated, Index, _Id, _Frame, State) ->
    cancelTimer(State#guiState.delayedUpdate),
    ETodo = getETodoAtIndex(Index, State#guiState.rows),
    State2 = updateGui(ETodo, Index, State),
    drillDown(State2);
mainTaskListEvent(command_list_col_right_click,
                  Col, _Id, Frame, State = #guiState{user    = User,
                                                     columns = Columns}) ->
    {value, {Col, Column}} = lists:keysearch(Col, 1, Columns),
    showMenu(User, Column, Frame, State);
mainTaskListEvent(command_list_item_right_click,
                  Row, _Id, Frame, State = #guiState{user    = User}) ->
    showMenu(User, {row, Row}, Frame, State).

drillDown(State = #guiState{activeTodo = {ETodo, _Ind}, drillDown = []}) ->
    Back    = xrcId("backTool"),
    ToolBar = State#guiState.toolBar,
    wxToolBar:enableTool(ToolBar, Back, true),
    State2 = State#guiState{drillDown     = [ETodo#etodo.uidDB],
                            drillFromList = getTaskList(State)},
    State3 = updateTodoWindow(State2#guiState{activeTodo = {undefined, -1}}),
    focusAndSelect(State3);

drillDown(State = #guiState{activeTodo = {ETodo, _Index},
                            drillDown  = DrillDown}) ->
    Back    = xrcId("backTool"),
    ToolBar = State#guiState.toolBar,
    wxToolBar:enableTool(ToolBar, Back, true),
    State2 = State#guiState{drillDown = [ETodo#etodo.uidDB|DrillDown]},
    State3 = updateTodoWindow(State2#guiState{activeTodo = {undefined, -1}}),
    focusAndSelect(State3).

hasSubTodo(State) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    case findSelected(TodoList) of
        Index when Index >= 0 ->
            ETodo = getETodoAtIndex(Index, State#guiState.rows),
            ETodo#etodo.hasSubTodo;
        _ ->
            false
    end.

%%====================================================================
%% Timer callbacks
%%====================================================================

timerOkEvent(_Type, _Id, _Frame, State = #guiState{timerDlg = TimerDlg,
                                                   timerRef = OldTimerRef,
                                                   user     = User}) ->
    Obj1 = wxXmlResource:xrcctrl(TimerDlg, "hourTimer",      wxSpinCtrl),
    Obj2 = wxXmlResource:xrcctrl(TimerDlg, "minTimer",       wxSpinCtrl),
    Obj3 = wxXmlResource:xrcctrl(TimerDlg, "secTimer",       wxSpinCtrl),
    Obj4 = wxXmlResource:xrcctrl(TimerDlg, "executeTimCmd",  wxCheckBox),
    Obj5 = wxXmlResource:xrcctrl(TimerDlg, "execTimCmd",     wxTextCtrl),
    Obj6 = wxXmlResource:xrcctrl(TimerDlg, "timerDesc",      wxTextCtrl),

    Hours   = wxSpinCtrl:getValue(Obj1),
    Minutes = wxSpinCtrl:getValue(Obj2),
    Seconds = wxSpinCtrl:getValue(Obj3),

    MSeconds = ((Hours * 3600) + (Minutes * 60) + Seconds) * 1000,
    MsgTxt   = wxTextCtrl:getValue(Obj6),

    Message =
        case wxCheckBox:isChecked(Obj4) of
            true ->
                {timerEnded, MsgTxt, wxTextCtrl:getValue(Obj5)};
            false ->
                {timerEnded, MsgTxt}
        end,

    TimerRef = erlang:send_after(MSeconds, self(), Message),

    if OldTimerRef =/= undefined -> ePluginServer:eTimerStopped(User);
       true ->
            UserCfg1 = eTodoDB:readUserCfg(User),
            UserCfg2 = UserCfg1#userCfg{lastTimer = {Hours, Minutes, Seconds}},
            eTodoDB:saveUserCfg(UserCfg2)
    end,

    ePluginServer:eTimerStarted(User, MsgTxt, Hours, Minutes, Seconds),
    eWeb:setTimerRef(User, TimerRef),

    wxDialog:hide(TimerDlg),
    cancelTimer(OldTimerRef),
    State#guiState{timerRef = TimerRef, timerDlgOpen = false}.

timerCancelEvent(_Type, _Id, _Frame, State = #guiState{timerDlg = TimerDlg,
                                                       timerRef = TimerRef,
                                                       user     = User}) ->
    wxDialog:hide(TimerDlg),
    cancelTimer(TimerRef),

    ePluginServer:eTimerStopped(User),
    eWeb:setTimerRef(User, undefined),

    State#guiState{timerRef = undefined, timerDlgOpen = false}.

timerToolEvent(_Type, _Id, _Frame, State = #guiState{timerDlg = TimerDlg,
                                                     timerRef = undefined,
                                                     user     = User}) ->
    Obj1 = wxXmlResource:xrcctrl(TimerDlg, "hourTimer", wxSpinCtrl),
    Obj2 = wxXmlResource:xrcctrl(TimerDlg, "minTimer",  wxSpinCtrl),
    Obj3 = wxXmlResource:xrcctrl(TimerDlg, "secTimer",  wxSpinCtrl),
    Obj4 = wxXmlResource:xrcctrl(TimerDlg, "teamPom",   wxCheckBox),

    UserCfg = eTodoDB:readUserCfg(User),
    {Hours, Minutes, Seconds} = default(UserCfg#userCfg.lastTimer, {0, 25, 0}),

    wxSpinCtrl:setValue(Obj1, Hours),
    wxSpinCtrl:setValue(Obj2, Minutes),
    wxSpinCtrl:setValue(Obj3, Seconds),

    wxDialog:show(TimerDlg),

    self()!{setPomodoroClock, Obj1, Obj2, Obj3, Obj4},
    State#guiState{timerDlgOpen = true};
timerToolEvent(_Type, _Id, _Frame, State = #guiState{timerDlg = TimerDlg,
                                                     timerRef = TimerRef}) ->
    Obj1 = wxXmlResource:xrcctrl(TimerDlg, "hourTimer", wxSpinCtrl),
    Obj2 = wxXmlResource:xrcctrl(TimerDlg, "minTimer",  wxSpinCtrl),
    Obj3 = wxXmlResource:xrcctrl(TimerDlg, "secTimer",  wxSpinCtrl),
    Obj4 = wxXmlResource:xrcctrl(TimerDlg, "teamPom",   wxCheckBox),

    MSeconds = erlang:read_timer(TimerRef),

    Hours    = (MSeconds div (3600*1000)),
    Minutes  = (MSeconds div (60*1000)) - Hours * 60,
    Seconds  = (MSeconds div 1000) - Hours * 3600 - Minutes * 60,

    wxSpinCtrl:setValue(Obj1, Hours),
    wxSpinCtrl:setValue(Obj2, Minutes),
    wxSpinCtrl:setValue(Obj3, Seconds),

    wxDialog:show(TimerDlg),

    self()!{setPomodoroClock, Obj1, Obj2, Obj3, Obj4},
    State#guiState{timerDlgOpen = true}.

%%====================================================================
%% Toolbar Create new task at top of task list
%%====================================================================
todoUpToolEvent(_Type, _Id, _Frame, State = #guiState{user    = User,
                                                      columns = Columns}) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    Todo     = #todo{uid = makeRef(), createTime = {date(), time()}},
    eTodoDB:insertTodo(#userInfo{userName = User,
                                 uid      = Todo#todo.uid,
                                 row      = 0,
                                 parent   = TaskList}, Todo),
    ETodo  = makeETodo(Todo, User, Columns),
    State2 = addTodo(TodoList, ETodo, 0, State),
    State3 = updateGui(ETodo, 0, State2),
    State4 = focusAndSelect(0, State3),
    DescObj = obj("descriptionArea", State4),
    wxChoice:setFocus(DescObj),
    State4.

%%====================================================================
%% Toolbar Create new task at bottom of task list
%%====================================================================
addTaskMenuEvent(Type, Id, Frame, State) ->
    todoDownToolEvent(Type, Id, Frame, State).

addTaskInboxMenuEvent(_Type, _Id, _Frame,
                      State = #guiState{user = User, columns = Columns}) ->
    TaskList  = getTaskList(State),
    TodoList  = getTodoList(TaskList, State),
    Row       = wxListCtrl:getItemCount(TodoList),
    Todo      = #todo{uid = makeRef(), createTime = {date(), time()}},

    eTodoDB:addTodo(#userInfo{userName = User,
                              uid      = Todo#todo.uid,
                              row      = eTodoDB:getRow(User, ?defTaskList),
                              parent   = ?defTaskList}, Todo),
    ETodo  = makeETodo(Todo, User, Columns),
    State2 = addTodo(TodoList, ETodo, Row, State),
    State3 = updateGui(ETodo, Row, State2),
    State4 = focusAndSelect(Row, State3),
    DescObj = obj("descriptionArea", State4),
    wxChoice:setFocus(DescObj),
    State4.

todoDownToolEvent(_Type, _Id, _Frame, State = #guiState{user    = User,
                                                        columns = Columns}) ->
    TaskList  = getTaskList(State),
    TodoList  = getTodoList(TaskList, State),
    Row       = wxListCtrl:getItemCount(TodoList),
    Todo      = #todo{uid = makeRef(), createTime = {date(), time()}},
    eTodoDB:addTodo(#userInfo{userName = User,
                              uid      = Todo#todo.uid,
                              row      = eTodoDB:getRow(User, TaskList),
                              parent   = TaskList}, Todo),
    ETodo  = makeETodo(Todo, User, Columns),
    State2 = addTodo(TodoList, ETodo, Row, State),
    State3 = updateGui(ETodo, Row, State2),
    State4 = focusAndSelect(Row, State3),
    DescObj = obj("descriptionArea", State3),
    wxChoice:setFocus(DescObj),
    State4.

%%====================================================================
%% Toolbar delete button
%%====================================================================
deleteMenuEvent(_Type, _Id, _Frame, State) ->
    doDelete(State).

deleteToolEvent(_Type, _Id, _Frame, State) ->
    doDelete(State).

doDelete(State = #guiState{user = User}) ->
    TaskList  = getTaskList(State),
    TodoList  = getTodoList(TaskList, State),
    case findSelected(TodoList) of
        Index when Index >= 0 ->
            ETodo = getETodoAtIndex(Index, State#guiState.rows),

            %% Remove from GUI
            wxListCtrl:deleteItem(TodoList, Index),

            %% Remove from internal data structure.
            Rows2  = eRows:deleteRow(ETodo#etodo.uidDB, State#guiState.rows),
            State2 = State#guiState{rows = Rows2},

            %% Remove task from database.
            eTodoDB:delTodo(ETodo#etodo.uidDB, User),

            %% Select correct task
            State3 = deleteAndUpdate(Index, TodoList, State2),

            focusAndSelect(Index, State3#guiState{activeTodo = {undefined, -1}});
        _ ->
            focusAndSelect(0, State)
    end.

findSelected(TodoList) ->
    wxListCtrl:getNextItem(TodoList, -1, [{state, ?wxLIST_STATE_SELECTED}]).

%%====================================================================
%% Event to trigger gui save
%%====================================================================
dueDatePickerEvent(_Type, _Id, _Frame, State) ->
    State.

dueDateUsedEvent(_Type, _Id, _Frame, State) ->
    State.

priorityChoiceEvent(_Type, _Id, _Frame, State) ->
    State.

statusChoiceEvent(_Type, _Id, _Frame, State) ->
    State.

commentAreaEvent(_Type, _Id, _Frame, State) ->
    State.

descriptionAreaEvent(_Type, _Id, _Frame, State) ->
    State.

ownerChoiceEvent(_Type, _Id, _Frame, State) ->
    State.

progressInfoEvent(_Type, _Id, _Frame, State) ->
    State.

taskListChoiceEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    Back    = xrcId("backTool"),
    ToolBar = State#guiState.toolBar,
    wxToolBar:enableTool(ToolBar, Back, false),
    State2 =
        case getTaskList(State#guiState{drillDown = []}) of
            ?subTaskList ++ _ ->
                updateTodoWindow(State#guiState{activeTodo = {undefined, -1}});
            TaskList ->
                UserCfg = eTodoDB:readUserCfg(User),
                eTodoDB:saveUserCfg(UserCfg#userCfg{lastTaskList = TaskList}),
                updateTodoWindow(State#guiState{activeTodo = {undefined, -1},
                                                drillDown  = []})
        end,
    focusAndSelect(State2).

%%====================================================================
%% Share task with users.
%%====================================================================
shareButtonEvent(_Type, _Id, _Frame,
                 State = #guiState{usersDlg   = Users,
                                   activeTodo = {ETodo, _},
                                   user       = User}) ->
    Obj = wxXmlResource:xrcctrl(Users, "userCheckListBox", wxCheckListBox),
    wxCheckListBox:clear(Obj),
    #userCfg{ownerCfg = OwnerCfg} = eTodoDB:readUserCfg(User),
    PeerList = eTodoDB:getUsers() ++ default(OwnerCfg, []),
    [wxCheckListBox:append(Obj, Peer) || Peer <- PeerList],
    checkItemsInList(Obj, default(ETodo#etodo.sharedWithDB, [User])),
    wxDialog:show(Users),
    State.

userOkEvent(_Type, _Id, _Frame,
            State = #guiState{user       = User,
                              usersDlg   = Users,
                              activeTodo = {ETodo, Index}}) ->
    wxDialog:hide(Users),
    Obj      = wxXmlResource:xrcctrl(Users, "userCheckListBox", wxCheckListBox),
    UserList = getCheckedItems(Obj),
    ETodo2   = ETodo#etodo{sharedWithDB = UserList,
                           sharedWith   = makeStr(UserList)},
    updateTodoInDB(User, ETodo2),
    SharedObj = obj("sharedWithText", State),
    wxStaticText:setLabel(SharedObj, ETodo2#etodo.sharedWith),
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    updateTodo(TodoList, ETodo2, Index, State).

userCancelEvent(_Type, _Id, _Frame, State = #guiState{usersDlg = Users}) ->
    wxDialog:hide(Users),
    State.

userCheckBoxEvent(_Type, _Id, _Frame, State) ->
    setPeerStatusIfNeeded(State).

userStatusChoiceEvent(_Type, _Id, _Frame, State) ->
    userStatusUpdate(State).

userStatusMsgEvent(_Type, _Id, _Frame, State) ->
    userStatusUpdate(State).


%%====================================================================
%% Manage bookmarks
%%====================================================================
addBookmarkButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{manBookmDlg = Manage,
                                         bookmCfg    = BookmCfg}) ->
    Obj = wxXmlResource:xrcctrl(Manage, "manageBookmarkBox", wxListBox),
    wxListBox:clear(Obj),
    [wxListBox:append(Obj, Bookm) || {Bookm, _Cfg} <- default(BookmCfg, [])],
    wxDialog:setSize(Manage, {250, 250}),
    wxDialog:show(Manage),
    State#guiState{bookmCfg = default(BookmCfg, [])}.

createBookmarkButtonEvent(_Type, _Id, _Frame,
                          State = #guiState{manBookmDlg = Manage,
                                            filter      = Filter,
                                            searchCfg   = SearchCfg,
                                            bookmCfg    = BookmCfg}) ->
    Obj1  = wxXmlResource:xrcctrl(Manage, "manageBookmarkBox", wxListBox),
    Obj2  = wxXmlResource:xrcctrl(Manage, "createBookmarkTxt", wxTextCtrl),
    Bookm = getItems(Obj1),
    NewBookm = wxTextCtrl:getValue(Obj2),
    wxTextCtrl:setValue(Obj2, ""),
    case NewBookm of
        "" ->
            State;
        NewBookm ->
            case lists:member(NewBookm, Bookm) of
                false ->
                    wxListBox:append(Obj1, NewBookm),
                    TaskList = getTaskList(State),
                    SearchText = wxComboBox:getValue(obj("searchText", State)),
                    NewBookmark = {NewBookm, {TaskList, Filter,
                                              SearchCfg, SearchText}},
                    State#guiState{bookmCfg = [NewBookmark|BookmCfg]};
                true ->
                    State
            end
    end.

manageBookmarkOkEvent(_Type, _Id, _Frame,
                      State = #guiState{manBookmDlg = Manage,
                                        user        = User,
                                        bookmCfg    = BookmCfg}) ->
    UserCfg  = eTodoDB:readUserCfg(User),
    UserCfg2 = UserCfg#userCfg{bookmCfg = default(BookmCfg, [])},
    eTodoDB:saveUserCfg(UserCfg2),
    wxDialog:hide(Manage),
    State.

updateBookmarkButtonEvent(_Type, _Id, _Frame,
                          State = #guiState{manBookmDlg = Manage,
                                            bookmCfg    = BookmCfg}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageBookmarkBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createBookmarkTxt", wxTextCtrl),
    {_, [Index]} = wxListBox:getSelections(Obj),
    OldValue = listBoxGet(Obj, Index),
    NewValue = wxTextCtrl:getValue(Obj2),
    wxListBox:setString(Obj, Index, NewValue),
    wxTextCtrl:setValue(Obj2, ""),
    NewName = fun({Name, Cfg}) -> case Name of
                                      OldValue ->
                                          {NewValue, Cfg};
                                      Else ->
                                          {Else, Cfg}
                                  end
              end,
    State#guiState{bookmCfg = lists:map(NewName, BookmCfg)}.


removeBookmarkButtonEvent(_Type, _Id, _Frame,
                          State = #guiState{manBookmDlg = Manage,
                                            bookmCfg    = BookmCfg}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageBookmarkBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createBookmarkTxt", wxTextCtrl),
    case wxListBox:getSelections(Obj) of
        {_, [Index]} ->
            ValueToRemove = listBoxGet(Obj, Index),
            BookmCfg2 = lists:keydelete(ValueToRemove, 1, BookmCfg),
            wxListBox:delete(Obj, Index),
            wxTextCtrl:setValue(Obj2, ""),
            State#guiState{bookmCfg = BookmCfg2};
        _ ->
            State
    end.

manageBookmarkCancelEvent(_Type, _Id, _Frame,
                          State = #guiState{manBookmDlg = Manage,
                                            user        = User}) ->
    wxDialog:hide(Manage),
    UserCfg  = eTodoDB:readUserCfg(User),
    State#guiState{bookmCfg = default(UserCfg#userCfg.bookmCfg, [])}.

%%====================================================================
%% Sort columns.
%%====================================================================
sortColumnsOkEvent(_Type, _Id, _Frame,
                   State = #guiState{sortColsDlg = SortCols,
                                     user        = User}) ->
    Obj  = wxXmlResource:xrcctrl(SortCols, "sortColumnsBox", wxListBox),
    updateOrder(User, getItems(Obj)),
    wxDialog:hide(SortCols),
    Columns  = eTodoDB:getColumns(User),
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    wxListCtrl:clearAll(TodoList),
    [wxListCtrl:deleteColumn(TodoList, Col) || {Col, _Desc} <- Columns],
    [wxListCtrl:insertColumn(TodoList, Col, Desc) || {Col, Desc} <- Columns],
    State2 = updateTodoWindow(State#guiState{columns = Columns}),
    [setColumnWidth(TodoList, User, Col, Desc) || {Col, Desc} <- Columns],
    State2.

updateOrder(User, Items) ->
    updateOrder(User, Items, 0).

updateOrder(_User, [], _Order) ->
    ok;
updateOrder(User, [Item|Rest], Order) ->
    eTodoDB:saveListCfg(User, taskInternal(Item), order, Order),
    updateOrder(User, Rest, Order + 1).

sortColumnsCancelEvent(_Type, _Id, _Frame,
                       State = #guiState{sortColsDlg = SortCols}) ->
    wxDialog:hide(SortCols),
    State.

moveColUpButtonEvent(Type, Id, Frame,
                     State = #guiState{sortColsDlg = SortCols}) ->
    Obj  = wxXmlResource:xrcctrl(SortCols, "sortColumnsBox", wxListBox),
    {_, [Index]} = wxListBox:getSelections(Obj),
    MovedCol1 = listBoxGet(Obj, Index),
    MovedCol2 = listBoxGet(Obj, Index - 1),
    wxListBox:setString(Obj, Index, MovedCol2),
    wxListBox:setString(Obj, Index - 1, MovedCol1),
    wxListBox:select(Obj, Index - 1),
    sortColumnsBoxEvent(Type, Id, Frame, State).

moveColDownButtonEvent(Type, Id, Frame,
                       State = #guiState{sortColsDlg = SortCols}) ->
    Obj  = wxXmlResource:xrcctrl(SortCols, "sortColumnsBox", wxListBox),
    {_, [Index]} = wxListBox:getSelections(Obj),
    MovedCol1 = listBoxGet(Obj, Index),
    MovedCol2 = listBoxGet(Obj, Index + 1),
    wxListBox:setString(Obj, Index, MovedCol2),
    wxListBox:setString(Obj, Index + 1, MovedCol1),
    wxListBox:select(Obj, Index + 1),
    sortColumnsBoxEvent(Type, Id, Frame, State).

sortColumnsBoxEvent(_Type, _Id, _Frame,
                    State = #guiState{sortColsDlg = SortCols}) ->
    Obj  = wxXmlResource:xrcctrl(SortCols, "sortColumnsBox",    wxListBox),
    Obj2 = wxXmlResource:xrcctrl(SortCols, "moveColUpButton",   wxButton),
    Obj3 = wxXmlResource:xrcctrl(SortCols, "moveColDownButton", wxButton),
    Num  = wxListBox:getCount(Obj),
    case wxListBox:getSelections(Obj) of
        {_, [0]} when Num == 0 ->
            wxButton:disable(Obj2),
            wxButton:disable(Obj3);
        {_, [0]} when Num > 0 ->
            wxButton:disable(Obj2),
            wxButton:enable(Obj3);
        {_, [Index]} when Index < (Num - 1) ->
            wxButton:enable(Obj2),
            wxButton:enable(Obj3);
        {_, [Index]} when Index == (Num - 1) ->
            wxButton:enable(Obj2),
            wxButton:disable(Obj3);
        _ ->
            wxButton:disable(Obj2),
            wxButton:disable(Obj3)
    end,
    State.

%%====================================================================
%% Smiley buttons
%%====================================================================
smileyHappyEvent(_Type, _Id, _Frame, State) ->
    insertMsgText(":-)", State).

smileyLOLEvent(_Type, _Id, _Frame, State) ->
    insertMsgText(":D", State).

smileyMischiefEvent(_Type, _Id, _Frame, State) ->
    insertMsgText(":P", State).

smileyShockedEvent(_Type, _Id, _Frame, State) ->
    insertMsgText(":O", State).

smileyCryingEvent(_Type, _Id, _Frame, State) ->
    insertMsgText(":,(", State).

smileyWinkEvent(_Type, _Id, _Frame, State) ->
    insertMsgText(";-)", State).

smileyHeartEvent(_Type, _Id, _Frame, State) ->
    insertMsgText("<3", State).

linkFileButtonEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    PortStr = toStr(eWeb:getPort()),

    case {getFileToLink(State), PortStr} of
        {{?wxID_OK, Path, File}, PortStr} when PortStr =/= "-1" ->
            Reference    = toStr(makeRef()),
            Args         =
                "?filename="  ++ http_uri:encode(File) ++
                "&reference=" ++ http_uri:encode(Reference),

            ConCfg = default(eTodoDB:getConnection(User),
                             #conCfg{host = "localhost"}),
            Host   = default(ConCfg#conCfg.host, "localhost"),

            UserCfg  = eTodoDB:readUserCfg(State#guiState.user),

            {Host2, Port2, ProxyArg} =
                case UserCfg#userCfg.webProxyEnabled of
                    true ->
                        {UserCfg#userCfg.webProxyHost,
                         toStr(UserCfg#userCfg.webProxyPort),
                         "&proxy=" ++ User};
                    false ->
                        {Host, PortStr, ""}
                end,

            {ok, Bin} = file:read_file(Path),
            ZBin      = zlib:gzip(Bin),
            FileName  = filename:join([getRootDir(), "www", "linkedFiles",
                                       Reference ++ "_" ++ File]),
            filelib:ensure_dir(FileName),
            file:write_file(FileName, ZBin),
            MimeType    = eTodoUtils:mime_type(File),
            Disposition = eTodoUtils:getDisposition(MimeType, File),
            Url  = "https://" ++ Host2 ++ ":" ++ Port2 ++
                "/eTodo/eWeb:link" ++ Args ++ ProxyArg,
            Link = case Disposition of
                       "inline" ->
                            "![" ++ File ++ "](" ++ Url ++ ")";
                       _ ->
                            "[" ++ File ++ "](" ++ Url ++ ")"
                   end,
            insertMsgText(Link, State);
        _->
            eTodo:systemEntry(system, "Failed to link file, "
                              "web server not running"),
            State
    end.

insertMsgText(Text, State) ->
    Obj1 = obj("msgTextCtrl", State),
    Obj2 = obj("msgNotebook", State),
    wxNotebook:changeSelection(Obj2, 0),
    wxTextCtrl:appendText(Obj1, Text),
    wxTextCtrl:setFocus(Obj1),
    State.

%%====================================================================
%% Manage task owners.
%%====================================================================
addOwnerButtonEvent(_Type, _Id, _Frame,
                    State = #guiState{manOwnerDlg = Manage,
                                      user        = User}) ->
    Obj = wxXmlResource:xrcctrl(Manage, "manageOwnerBox", wxListBox),
    #userCfg{ownerCfg = OwnerCfg} = eTodoDB:readUserCfg(User),
    wxListBox:clear(Obj),
    [wxListBox:append(Obj, Owner) || Owner <- default(OwnerCfg, [])],
    wxDialog:setSize(Manage, {250, 250}),
    wxDialog:show(Manage),
    State.

createOwnerButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{manOwnerDlg = Manage}) ->
    Obj1 = wxXmlResource:xrcctrl(Manage, "manageOwnerBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createOwnerTxt", wxTextCtrl),
    Owns = getItems(Obj1),
    NewOwner = wxTextCtrl:getValue(Obj2),
    wxTextCtrl:setValue(Obj2, ""),
    case NewOwner of
        "" ->
            State;
        NewOwner ->
            case lists:member(NewOwner, Owns) of
                false ->
                    wxListBox:append(Obj1, NewOwner),
                    State;
                true ->
                    State
            end
    end.


removeOwnerButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{manOwnerDlg = Manage}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageOwnerBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createOwnerTxt", wxTextCtrl),
    case wxListBox:getSelections(Obj) of
        {_, [Index]} ->
            wxListBox:delete(Obj, Index),
            wxTextCtrl:setValue(Obj2, "");
        _ ->
            ok
    end,
    State.

updateOwnerButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{manOwnerDlg = Manage}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageOwnerBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createOwnerTxt", wxTextCtrl),
    {_, [Index]} = wxListBox:getSelections(Obj),
    NewValue = wxTextCtrl:getValue(Obj2),
    wxListBox:setString(Obj, Index, NewValue),
    wxTextCtrl:setValue(Obj2, ""),
    State.

manageBookmarkBoxEvent(_Type, _Id, _Frame,
                       State = #guiState{manBookmDlg = Manage}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageBookmarkBox",    wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createBookmarkTxt",    wxTextCtrl),
    Obj3 = wxXmlResource:xrcctrl(Manage, "updateBookmarkButton", wxButton),
    case wxListBox:getSelections(Obj) of
        {_, [Index]} ->
            Value = listBoxGet(Obj, Index),
            wxTextCtrl:setValue(Obj2, Value),
            wxButton:enable(Obj3);
        _ ->
            wxButton:disable(Obj3)
    end,
    State.

manageOwnerBoxEvent(_Type, _Id, _Frame,
                    State = #guiState{manOwnerDlg = Manage}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageOwnerBox",    wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createOwnerTxt",    wxTextCtrl),
    Obj3 = wxXmlResource:xrcctrl(Manage, "updateOwnerButton", wxButton),
    case wxListBox:getSelections(Obj) of
        {_, [Index]} ->
            Value = listBoxGet(Obj, Index),
            wxTextCtrl:setValue(Obj2, Value),
            wxButton:enable(Obj3);
        _ ->
            wxButton:disable(Obj3)
    end,
    State.

manageOwnerOkEvent(_Type, _Id, _Frame,
                   State = #guiState{manOwnerDlg = Manage,
                                     user        = User,
                                     activeTodo = {ETodo, _Index}}) ->
    Obj      = wxXmlResource:xrcctrl(Manage, "manageOwnerBox", wxListBox),
    Owners   = getItems(Obj),
    UserCfg  = eTodoDB:readUserCfg(User),
    UserCfg2 = UserCfg#userCfg{ownerCfg = Owners},
    eTodoDB:saveUserCfg(UserCfg2),
    wxDialog:hide(Manage),

    setOwner(obj("ownerChoice", State), User, ETodo#etodo.owner),
    State.

manageOwnerCancelEvent(_Type, _Id, _Frame,
                       State = #guiState{manOwnerDlg = Manage}) ->
    wxDialog:hide(Manage),
    State.

%%====================================================================
%% Manage task lists.
%%====================================================================
manageListsButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{manListsDlg = Manage,
                                         user        = User}) ->
    Obj = wxXmlResource:xrcctrl(Manage, "manageListBox", wxListBox),
    TodoLists = getTodoLists(User),
    wxListBox:clear(Obj),
    [wxListBox:append(Obj, List) || List <- TodoLists],
    wxDialog:setSize(Manage, {250, 250}),
    wxDialog:show(Manage),
    State#guiState{updList = []}.

createListButtonEvent(_Type, _Id, _Frame,
                      State = #guiState{manListsDlg = Manage}) ->
    Obj1  = wxXmlResource:xrcctrl(Manage, "manageListBox", wxListBox),
    Obj2  = wxXmlResource:xrcctrl(Manage, "createListTxt", wxTextCtrl),
    Lists = getItems(Obj1),
    NewList = wxTextCtrl:getValue(Obj2),
    wxTextCtrl:setValue(Obj2, ""),
    case NewList of
        "" ->
            State;
        ?defTaskList ->
            State;
        NewList ->
            case catch list_to_integer(NewList) of
                {'EXIT', _} ->
                    case lists:member(NewList, [?defInbox,
                                                ?defLoggedWork| Lists]) of
                        false ->
                            wxListBox:append(Obj1, NewList),
                            State;
                        true ->
                            State
                    end;
                _ ->
                    %% Do not allow lists named as integer, it could
                    %% conflict with a sublist.
                    State
            end
    end.

removeListButtonEvent(_Type, _Id, _Frame,
                      State = #guiState{manListsDlg = Manage}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageListBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createListTxt", wxTextCtrl),
    case wxListBox:getSelections(Obj) of
        {_, [Index]} ->
            case listBoxGet(Obj, Index) of
                ?defTaskList ->
                    ok;
                _ ->
                    wxTextCtrl:setValue(Obj2, ""),
                    wxListBox:delete(Obj, Index)
            end;
        _ ->
            ok
    end,
    State.

updateListButtonEvent(_Type, _Id, _Frame,
                      State = #guiState{manListsDlg = Manage, updList = Upd}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageListBox", wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createListTxt", wxTextCtrl),
    {_, [Index]} = wxListBox:getSelections(Obj),
    Lists = getItems(Obj),
    OldValue = listBoxGet(Obj, Index),
    NewValue = wxTextCtrl:getValue(Obj2),
    case catch list_to_integer(NewValue) of
        {'EXIT', _} ->
            case lists:member(NewValue, [?defInbox,
                                         ?defLoggedWork,
                                         ?defShowStatus | Lists]) of
                false ->
                    wxListBox:setString(Obj, Index, NewValue),
                    wxTextCtrl:setValue(Obj2, ""),
                    State#guiState{updList = Upd ++ [{OldValue, NewValue}]};
                true ->
                    State
            end;
        _ ->
            State
    end.

manageListBoxEvent(_Type, _Id, _Frame,
                   State = #guiState{manListsDlg = Manage}) ->
    Obj  = wxXmlResource:xrcctrl(Manage, "manageListBox",    wxListBox),
    Obj2 = wxXmlResource:xrcctrl(Manage, "createListTxt",    wxTextCtrl),
    Obj3 = wxXmlResource:xrcctrl(Manage, "updateListButton", wxButton),
    case wxListBox:getSelections(Obj) of
        {_, [Index]} ->
            case listBoxGet(Obj, Index) of
                ?defTaskList ->
                    wxButton:disable(Obj3);
                Value ->
                    wxTextCtrl:setValue(Obj2, Value),
                    wxButton:enable(Obj3)
            end;
        _ ->
            wxButton:disable(Obj3)
    end,
    State.

manageListOkEvent(_Type, _Id, _Frame,
                  State = #guiState{manListsDlg = Manage,
                                    updList     = Upd,
                                    user        = User}) ->
    wxDialog:hide(Manage),
    [eTodoDB:moveTodosToTaskList(User, To, [From]) || {From, To} <- Upd],
    Obj   = wxXmlResource:xrcctrl(Manage, "manageListBox", wxListBox),
    Lists = getItems(Obj),
    setTaskLists(Lists, State),
    updateTodoWindow(State).

manageListCancelEvent(_Type, _Id, _Frame,
                      State = #guiState{manListsDlg = Manage}) ->
    wxDialog:hide(Manage),
    State.

addListButtonEvent(_Type, _Id, _Frame,
                   State = #guiState{addListDlg = AddList,
                                     user       = User,
                                     activeTodo = {ETodo, _}}) ->
    Obj = wxXmlResource:xrcctrl(AddList, "listCheckListBox", wxCheckListBox),
    wxCheckListBox:clear(Obj),
    TodoLists = getTodoLists(User),
    CheckedLists = eTodoDB:getLists(User, ETodo#etodo.uidDB),
    Extra = CheckedLists -- TodoLists,
    [wxCheckListBox:append(Obj, List)|| List <- TodoLists ++ Extra],
    checkItemsInList(Obj, CheckedLists),
    wxDialog:show(AddList),
    State.

listCheckListBoxEvent(_Type, _Id, _Frame,
                      State = #guiState{addListDlg = AddList}) ->
    Obj = wxXmlResource:xrcctrl(AddList, "listCheckListBox", wxCheckListBox),
    checkItemsInList(Obj, [?defTaskList]),
    State.

listOkEvent(_Type, _Id, _Frame,
            State = #guiState{addListDlg = AddList,
                              user       = User,
                              activeTodo = {ETodo, Index}}) ->
    TaskList = getTaskList(State),
    Obj   = wxXmlResource:xrcctrl(AddList, "listCheckListBox", wxCheckListBox),
    Lists = getCheckedItems(Obj),
    eTodoDB:assignLists(User, ETodo#etodo.uidDB, Lists),
    wxDialog:hide(AddList),
    ListsObj = obj("addedToLists", State),
    wxStaticText:setLabel(ListsObj, makeStr(Lists)),
    ETodo2 = ETodo#etodo{lists = makeStr(Lists), listsDB = Lists},
    case lists:member(TaskList, Lists) of
        true ->
            State2 = State#guiState{activeTodo = {ETodo2, Index}},
            Rows2  = updateRows(ETodo2, State2#guiState.rows),
            State2#guiState{rows = Rows2};
        false ->
            State2 = updateTodoWindow(State),
            focusAndSelect(State2#guiState{activeTodo = {undefined, -1}})
    end.

listCancelEvent(_Type, _Id, _Frame, State = #guiState{addListDlg = AddList}) ->
    wxDialog:hide(AddList),
    State.

%%====================================================================
%% Insert signature into comment field.
%%====================================================================
commentButtonEvent(_Type, _Id, _Frame, State) ->
    Signature = toStr(date()) ++ " " ++ State#guiState.user ++ ": ",
    CommentField = obj("commentArea", State),
    case wxTextCtrl:getValue(CommentField) of
        "" ->
            wxTextCtrl:appendText(CommentField, Signature),
            wxTextCtrl:setFocus(CommentField);
        _ ->
            wxTextCtrl:appendText(CommentField, "\r\n" ++ Signature),
            wxTextCtrl:setFocus(CommentField)
    end,
    wxNotebook:changeSelection(obj("commentNotebook", State), 0),
    State.

sendTaskButtonEvent(command_button_clicked, _Id, _Frame,
                    State = #guiState{activeTodo = {ETodo, _}, user = User}) ->
    Text  = ETodo#etodo.description,
    Users = ETodo#etodo.sharedWithDB,
    eTodo:systemEntry(ETodo#etodo.uidDB, Text),
    ePeerEM:sendMsg(User, Users, {systemEntry, ETodo#etodo.uidDB}, Text),
    State;
sendTaskButtonEvent(context_menu, _Id, _Frame,
    State = #guiState{activeTodo = {ETodo, _}, user = User}) ->
    Users = ETodo#etodo.sharedWithDB,
    UidStr = eTodoUtils:convertUid(ETodo#etodo.uidDB),
    Link   = eHtml:aTag([{href, UidStr}], "eTodo"),
    toClipboard(Link, State).

%%====================================================================
%% Log work button
%%====================================================================
logWorkButtonEvent(_Type, _Id, _Frame, State = #guiState{logWorkDlg = LWDlg,
                                                         user       = User}) ->
    HoursObj   = wxXmlResource:xrcctrl(LWDlg, "workHours",       wxSpinCtrl),
    MinutesObj = wxXmlResource:xrcctrl(LWDlg, "workMinutes",     wxSpinCtrl),
    DateObj    = wxXmlResource:xrcctrl(LWDlg, "workDate",        wxDatePickerCtrl),
    DescObj    = wxXmlResource:xrcctrl(LWDlg, "workDesc",        wxTextCtrl),
    EstObj     = wxXmlResource:xrcctrl(LWDlg, "timeEstimate",    wxSpinCtrl),
    SpentObj   = wxXmlResource:xrcctrl(LWDlg, "timeSpent",       wxStaticText),
    RemObj     = wxXmlResource:xrcctrl(LWDlg, "timeRemaining",   wxSpinCtrl),
    WLCheckB   = wxXmlResource:xrcctrl(LWDlg, "workLogCheckBox", wxCheckBox),
    TLCheckB   = wxXmlResource:xrcctrl(LWDlg, "timeLogCheckBox", wxCheckBox),

    {ETodo, _} = State#guiState.activeTodo,
    Uid        = ETodo#etodo.uid,
    Date2      = case wxDatePickerCtrl:getValue(DateObj) of
                     {{2001,1,1}, _} ->
                         Date = date(),
                         wxDatePickerCtrl:setValue(DateObj, {Date, {1,0,0}}),
                         Date;
                     {Date, _} ->
                         Date
                 end,
    wxDatePickerCtrl:setToolTip(DateObj, getWeekDay(Date2)),

    {Date2, Hours, Minutes} = eTodoDB:getLoggedWork(User, Uid, Date2),
    {Estimate, Remaining}  = eTodoDB:getTime(Uid),
    {ok, Desc, ShowInWorkLog, ShowInTimeLog} = eTodoDB:getWorkDescAll(Uid),
    Desc2 = getWorkDesc(Desc, ETodo#etodo.description),
    Spent = eTodoDB:getAllLoggedWork(Uid) ++ " h",
    LTime = eTodoDB:getAllLoggedWorkDate(Uid),
    wxStaticText:setToolTip(SpentObj, constructTooltip(LTime)),

    wxSpinCtrl:setValue(HoursObj,   Hours),
    wxSpinCtrl:setValue(MinutesObj, Minutes),
    wxTextCtrl:setValue(DescObj,    Desc2),
    wxSpinCtrl:setValue(EstObj,     Estimate),
    wxStaticText:setLabel(SpentObj, Spent),
    wxSpinCtrl:setValue(RemObj,     Remaining),
    wxCheckBox:setValue(WLCheckB,   default(ShowInWorkLog, false)),
    wxCheckBox:setValue(TLCheckB,   default(ShowInTimeLog, false)),

    wxDialog:show(LWDlg),

    wxSpinCtrl:setFocus(HoursObj),
    State.

logWorkCancelEvent(_Type, _Id, _Frame, State = #guiState{logWorkDlg = LWDlg}) ->
    wxDialog:hide(LWDlg),
    clearActive(State).

logWorkOkEvent(_Type, _Id, _Frame, State = #guiState{logWorkDlg = LWDlg,
                                                     user       = User}) ->
    HoursObj   = wxXmlResource:xrcctrl(LWDlg, "workHours",       wxSpinCtrl),
    MinutesObj = wxXmlResource:xrcctrl(LWDlg, "workMinutes",     wxSpinCtrl),
    DateObj    = wxXmlResource:xrcctrl(LWDlg, "workDate",        wxDatePickerCtrl),
    DescObj    = wxXmlResource:xrcctrl(LWDlg, "workDesc",        wxTextCtrl),
    EstObj     = wxXmlResource:xrcctrl(LWDlg, "timeEstimate",    wxSpinCtrl),
    RemObj     = wxXmlResource:xrcctrl(LWDlg, "timeRemaining",   wxSpinCtrl),
    WLCheckB   = wxXmlResource:xrcctrl(LWDlg, "workLogCheckBox", wxCheckBox),
    TLCheckB   = wxXmlResource:xrcctrl(LWDlg, "timeLogCheckBox", wxCheckBox),

    Hours      = wxSpinCtrl:getValue(HoursObj),
    Minutes    = wxSpinCtrl:getValue(MinutesObj),
    Estimate   = wxSpinCtrl:getValue(EstObj),
    Remaining  = wxSpinCtrl:getValue(RemObj),
    Desc       = wxTextCtrl:getValue(DescObj),
    DateTime   = wxDatePickerCtrl:getValue(DateObj),
    ShowInWL   = wxCheckBox:isChecked(WLCheckB),
    ShowInTL   = wxCheckBox:isChecked(TLCheckB),

    {Date, _}  = DateTime,
    {ETodo, _} = State#guiState.activeTodo,
    Uid        = ETodo#etodo.uid,

    eTodoDB:logWork(User, Uid, Date, Hours, Minutes),
    eTodoDB:saveTime(Uid, Estimate, Remaining),
    eTodoDB:saveWorkDesc(Uid, Desc, ShowInWL, ShowInTL),

    wxDialog:hide(LWDlg),
    State2 = eGuiFunctions:generateWorkLog(State),
    State3 = eGuiFunctions:generateTimeLog(State2),
    clearActive(State3).

workDateEvent(Type, Id, Frame, State) ->
    logWorkButtonEvent(Type, Id, Frame, State).

workLogStartDateEvent(_Type, _Id, _Frame, State) ->
    eGuiFunctions:generateWorkLog(State).

clearActive(State=#guiState{activeTodo = {_, -1}}) ->
    State#guiState{activeTodo = {undefined, -1}};
clearActive(State) ->
    State.
%%====================================================================
%% About
%%====================================================================
helpMenu1Event(_Type, _Id, _Frame, State) ->
    {ok, Cwd} = file:get_cwd(),
    FileUri   = getFileURI(),
    Dir       = FileUri ++ filename:join([Cwd, getRootDir(),
                                          "www", "docs", "eTodo.html"]),
    wx_misc:launchDefaultBrowser(re:replace(Dir, " ", "%20", [{return, list}])),
    State.

aboutMenuEvent(_Type, _Id, _Frame, State = #guiState{aboutDlg = About}) ->
    wxDialog:showModal(About),
    State.

getFileURI() ->
    case os:type() of
        {win32, _} ->
            "file:///";
        _->
            "file://"
    end.

%%====================================================================
%% Construct proxy link
%%====================================================================
proxyLinkMenuEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    UserCfg  = eTodoDB:readUserCfg(State#guiState.user),
    WebPwd   = default(UserCfg#userCfg.webPassword, ""),
    Token    = base64:encode_to_string(crypto:hash(sha, User ++ "@" ++ WebPwd)),
    {Host, Port} =
        case UserCfg#userCfg.webProxyEnabled of
            true ->
                {UserCfg#userCfg.webProxyHost,
                 toStr(UserCfg#userCfg.webProxyPort)};
            false ->
                {"ServerNameHere", "PortHere"}
        end,
    Args     =
        "?proxy="    ++ http_uri:encode(User)   ++
        "&token="    ++ http_uri:encode(Token),
    Link = "https://" ++ Host ++ ":" ++ Port ++ "/eTodo/eWeb:index" ++ Args,
    eTodo:systemEntry(system, Link ++ " Can be used to access web gui through "
                      "a proxy host. Link added to clipboard."),
    toClipboard(Link, State).

%%====================================================================
%% Link view
%%====================================================================
linkViewMenuEvent(_Type, _Id, _Frame, State = #guiState{searchCfg = Cfg,
                                                        filter    = Flt,
                                                        user      = User}) ->
    case toStr(eWeb:getPort()) of
        "-1" ->
            eTodo:systemEntry(system, "Failed to link view, "
                              "web server not running"),
            State;
        PortStr ->
            List      = toStr(getTaskList(State)),
            Search    = wxComboBox:getValue(obj("searchText", State)),
            SearchCfg = makeStr(Cfg),
            Filter    = makeStr(useFilter(getTaskList(State), Flt, State)),
            Args      =
                "?list="      ++ http_uri:encode(List)      ++
                "&search="    ++ http_uri:encode(Search)    ++
                "&searchCfg=" ++ http_uri:encode(SearchCfg) ++
                "&filter="    ++ http_uri:encode(Filter),

            ConCfg = default(eTodoDB:getConnection(User),
                             #conCfg{host = "localhost"}),
            Host   = default(ConCfg#conCfg.host, "localhost"),
            Link   = "https://" ++ Host ++ ":" ++ PortStr ++
                "/eTodo/eWeb:show" ++ Args,

            eTodo:systemEntry(system, Link ++ " shows configured view "
                              "in a browser. Link added to clipboard."),
            toClipboard(Link, State)
    end.

linkTimeReportMenuEvent(_Type, _Id, _Frame, State = #guiState{searchCfg = Cfg,
                                                              filter    = Flt,
                                                              user      = User}) ->
    case toStr(eWeb:getPort()) of
        "-1" ->
            eTodo:systemEntry(system, "Failed to link time report, "
                              "web server not running"),
            State;
        PortStr ->
            List      = toStr(getTaskList(State)),
            Search    = wxComboBox:getValue(obj("searchText", State)),
            SearchCfg = makeStr(Cfg),
            Filter    = makeStr(useFilter(getTaskList(State), Flt, State)),
            Args      =
                "?list="      ++ http_uri:encode(List)      ++
                "&search="    ++ http_uri:encode(Search)    ++
                "&searchCfg=" ++ http_uri:encode(SearchCfg) ++
                "&filter="    ++ http_uri:encode(Filter),

            ConCfg = default(eTodoDB:getConnection(User),
                             #conCfg{host = "localhost"}),
            Host   = default(ConCfg#conCfg.host, "localhost"),
            Link   = "https://" ++ Host ++ ":" ++ PortStr ++
                "/eTodo/eWeb:showTimeReport" ++ Args,

            eTodo:systemEntry(system, Link ++ " shows configured time "
                              "report in a browser. Link added to clipboard."),
            toClipboard(Link, State)
    end.

checkBoxUseFilterEvent(_Type, _Id, _Frame, State) ->
    updateTodoWindow(State).

linkFileMenuEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    PortStr = toStr(eWeb:getPort()),

    case {getFileToLink(State), PortStr} of
        {{?wxID_OK, Path, File}, PortStr} when PortStr =/= "-1" ->
            Reference    = toStr(makeRef()),
            Args         =
                "?filename="  ++ http_uri:encode(File) ++
                "&reference=" ++ http_uri:encode(Reference),

            ConCfg   = default(eTodoDB:getConnection(User),
                               #conCfg{host = "localhost"}),
            Host     = default(ConCfg#conCfg.host, "localhost"),
            UserCfg  = eTodoDB:readUserCfg(State#guiState.user),

            {Host2, Port2, ProxyArg} =
                case UserCfg#userCfg.webProxyEnabled of
                    true ->
                        {UserCfg#userCfg.webProxyHost,
                         toStr(UserCfg#userCfg.webProxyPort),
                         "&proxy=" ++ User};
                    false ->
                        {Host, PortStr, ""}
                end,

            {ok, Bin} = file:read_file(Path),
            ZBin      = zlib:gzip(Bin),
            FileName  = filename:join([getRootDir(), "www", "linkedFiles",
                                       Reference ++ "_" ++ File]),
            filelib:ensure_dir(FileName),
            file:write_file(FileName, ZBin),
            Link = "https://" ++ Host2 ++ ":" ++ Port2 ++
                "/eTodo/eWeb:link" ++ Args ++ ProxyArg,

            eTodo:systemEntry(system, Link ++
                                  " link to file added to clipboard."),
            toClipboard(Link, State);
        _->
            eTodo:systemEntry(system, "Failed to link file, "
                              "web server not running"),
            State
    end.

getFileToLink(#guiState{frame = Frame}) ->
    FileDlg = wxFileDialog:new(Frame, [{message, "Select file to link"}]),
    Result  = wxDialog:showModal(FileDlg),
    {Result,
     wxFileDialog:getPath(FileDlg),
     wxFileDialog:getFilename(FileDlg)}.

%%====================================================================
%% Copy
%%====================================================================
copyMenuEvent(_Type, _Id, _Frame, State) ->
    doCopy(State).

copyToolEvent(_Type, _Id, _Frame, State) ->
    doCopy(State).

doCopy(State = #guiState{rows = Rows}) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    case findSelected(TodoList) of
        Index when Index >= 0 ->
            ETodo = getETodoAtIndex(Index, Rows),
            State#guiState{clipBoard = ETodo};
        _ ->
            State
    end.

%%====================================================================
%% Cut
%%====================================================================
cutMenuEvent(_Type, _Id, _Frame, State) ->
    doCut(State).

cutToolEvent(_Type, _Id, _Frame, State) ->
    doCut(State).

doCut(State) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    case findSelected(TodoList) of
        Index when Index >= 0 ->
            {ETodo, State2} = delAndUpdate(Index, TodoList, TaskList, State),
            focusAndSelect(Index, State2#guiState{activeTodo = {undefined, -1},
                                                  clipBoard  = ETodo});
        _ ->
            State
    end.

delAndUpdate(Index, TodoList, TaskList, State = #guiState{rows = Rows,
                                                          user = User}) ->
    ETodo  = getETodoAtIndex(Index, Rows),
    wxListCtrl:deleteItem(TodoList, Index),
    Rows2  = eRows:deleteRow(ETodo#etodo.uidDB, State#guiState.rows),
    State2 = State#guiState{rows = Rows2},
    eTodoDB:delTodo(ETodo#etodo.uidDB, TaskList, User, false),
    State3 = deleteAndUpdate(Index, TodoList, State2),
    {ETodo, State3}.

%%====================================================================
%% Toolbar "Move down"
%%====================================================================
moveDownMenuEvent(Type, Id, Frame, State) ->
    moveDownToolEvent(Type, Id, Frame, State).

moveDownToolEvent(_Type, _Id, _Frame, State = #guiState{rows = Rows}) ->
    TaskList  = getTaskList(State),
    TodoList  = getTodoList(TaskList, State),
    Row       = wxListCtrl:getItemCount(TodoList),
    case findSelected(TodoList) of
        Index when Index < Row - 1 ->
            User  = State#guiState.user,
            ETodo1 = getETodoAtIndex(Index, Rows),
            ETodo2 = getETodoAtIndex(Index + 1, Rows),
            eTodoDB:moveDown(User, TaskList,
                             ETodo1#etodo.uidDB, ETodo2#etodo.uidDB),
            wxListCtrl:freeze(TodoList),
            State2 = updateTodo(TodoList, ETodo1, Index + 1, State),
            State3 = updateTodo(TodoList, ETodo2, Index,     State2),
            Rows2  = swapRows(ETodo1, ETodo2, State3#guiState.rows),
            wxListCtrl:ensureVisible(TodoList, Index + 1),
            wxListCtrl:setItemState(TodoList, Index + 1,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED),
            wxListCtrl:thaw(TodoList),
            State3#guiState{activeTodo = {ETodo1, Index + 1}, rows = Rows2};
        _ ->
            State
    end.

%%====================================================================
%% Toolbar "Forward"
%%====================================================================
forwardToolEvent(_Type, _Id, _Frame, State) ->
    drillDown(State).

%%====================================================================
%% Toolbar "Back"
%%====================================================================
backToolEvent(_Type, _Id, _Frame, State = #guiState{drillDown = [Uid]}) ->
    Back    = xrcId("backTool"),
    ToolBar = State#guiState.toolBar,
    wxToolBar:enableTool(ToolBar, Back, false),
    State2 = updateTodoWindow(State#guiState{drillDown = []}),
    {Index, _} = findIndex(Uid, State2#guiState.rows),
    focusAndSelect(Index, State2);
backToolEvent(_Type, _Id, _Frame, State = #guiState{drillDown = [Uid|Rest]}) ->
    Back    = xrcId("backTool"),
    ToolBar = State#guiState.toolBar,
    wxToolBar:enableTool(ToolBar, Back, true),
    State2 = updateTodoWindow(State#guiState{drillDown = Rest}),
    {Index, _} = findIndex(Uid, State2#guiState.rows),
    focusAndSelect(Index, State2).

%%====================================================================
%% Toolbar "Move first"
%%====================================================================
moveFirstMenuEvent(_Type, _Id, _Frame, State) ->
    case  delSelected(State) of
        {TaskList, TodoList, ETodo, State2} ->
            Todo = getTodo(ETodo),

            eTodoDB:insertTodo(#userInfo{userName = State#guiState.user,
                                         uid      = Todo#todo.uid,
                                         row      = 0,
                                         parent   = TaskList}, Todo),

            updateTodoWindow(State2),

            %% Make sure pasted task is selected after operation.
            wxListCtrl:setItemState(TodoList, 0,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED),
            State2;
        _ ->

            State
    end.

delSelected(State) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    case findSelected(TodoList) of
        Index when Index >= 0 ->
            {ETodo, State2} = delAndUpdate(Index, TodoList, TaskList, State),
            focusAndSelect(Index, State2#guiState{activeTodo = {undefined, -1}}),
            {TaskList, TodoList, ETodo, State2};
        _ ->
            State
    end.

getTodo(#etodo{statusDB     = Status,
               priorityDB   = Prio,
               uidDB        = Uid,
               doneTimeDB   = DoneTime,
               dueTimeDB    = DueTime,
               createTimeDB = CreateTime,
               description  = Description,
               comment      = Comment,
               progress     = Progress,
               sharedWithDB = SharedWith,
               owner        = Owner}) ->

    #todo{uid         = Uid,
          priority    = Prio,
          status      = Status,
          doneTime    = DoneTime,
          dueTime     = DueTime,
          createTime  = CreateTime,
          description = Description,
          comment     = Comment,
          progress    = Progress,
          sharedWith  = SharedWith,
          owner       = Owner}.

%%====================================================================
%% Toolbar "Move Last"
%%====================================================================
moveLastMenuEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    case delSelected(State) of
        {TaskList, TodoList, ETodo, State2} ->
            Row  = eTodoDB:getRow(User, TaskList),
            Todo = getTodo(ETodo),

            eTodoDB:addTodo(#userInfo{userName = User,
                                      uid      = Todo#todo.uid,
                                      row      = Row,
                                      parent   = tryInt(TaskList)}, Todo),

            updateTodoWindow(State2),

            %% Make sure pasted task is selected after operation.
            wxListCtrl:ensureVisible(TodoList, Row),
            wxListCtrl:setItemState(TodoList, Row,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED),
            State2;
        _ ->

            State
    end.

%%====================================================================
%% Toolbar "Move up"
%%====================================================================
moveUpMenuEvent(Type, Id, Frame, State) ->
    moveUpToolEvent(Type, Id, Frame, State).

moveUpToolEvent(_Type, _Id, _Frame, State = #guiState{rows = Rows}) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    case findSelected(TodoList) of
        Index when Index >= 1 ->
            User  = State#guiState.user,
            ETodo1 = getETodoAtIndex(Index - 1, Rows),
            ETodo2 = getETodoAtIndex(Index,     Rows),
            eTodoDB:moveUp(User, TaskList,
                           ETodo1#etodo.uidDB, ETodo2#etodo.uidDB),
            wxListCtrl:freeze(TodoList),
            State2 = updateTodo(TodoList, ETodo1, Index,     State),
            State3 = updateTodo(TodoList, ETodo2, Index - 1, State2),
            Rows2  = swapRows(ETodo1, ETodo2, State3#guiState.rows),
            wxListCtrl:ensureVisible(TodoList, Index - 1),
            wxListCtrl:setItemState(TodoList, Index - 1,
                                    ?wxLIST_STATE_SELECTED,
                                    ?wxLIST_STATE_SELECTED),
            wxListCtrl:thaw(TodoList),
            State3#guiState{activeTodo = {ETodo2, Index - 1}, rows = Rows2};
        _ ->
            State
    end.

%%====================================================================
%% Toolbar
%%====================================================================
pasteMenuEvent(_Type, _Id, _Frame, State) ->
    doPaste(State).

pasteToolEvent(_Type, _Id, _Frame, State) ->
    doPaste(State).

doPaste(State = #guiState{clipBoard = #etodo{statusDB     = Status,
                                             priorityDB   = Prio,
                                             uidDB        = Uid,
                                             doneTimeDB   = DoneTime,
                                             dueTimeDB    = DueTime,
                                             createTimeDB = CreateTime,
                                             description  = Description,
                                             comment      = Comment,
                                             progress     = Progress,
                                             sharedWithDB = SharedWith,
                                             owner        = Owner}}) ->
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),

    Row = case findSelected(TodoList) of
              Index when Index >= 0 ->
                  Index;
              _ ->
                  eTodoDB:getRow(State#guiState.user, TaskList)
          end,

    NewTodo = #todo{uid         = Uid,
                    priority    = Prio,
                    status      = Status,
                    doneTime    = DoneTime,
                    dueTime     = DueTime,
                    createTime  = CreateTime,
                    description = Description,
                    comment     = Comment,
                    progress    = Progress,
                    sharedWith  = SharedWith,
                    owner       = Owner},
    doPaste(TaskList, TodoList, NewTodo, Row, State);
doPaste(State) ->
    %% No item in clipBoard.
    State.

doPaste(TaskList, TodoList, Todo, Index, State = #guiState{user = User}) ->
    Todos = eTodoDB:getTodos(User, TaskList),
    Todo2 = case lists:keymember(Todo#todo.uid, #todo.uid, Todos) of
                true ->
                    %% The task exist in this list,
                    %% make a new ref and create time.
                    Todo#todo{uid = makeRef(), createTime = {date(), time()}};
                false ->
                    Todo
            end,
    eTodoDB:insertTodo(#userInfo{userName = User,
                                 uid      = Todo2#todo.uid,
                                 row      = Index,
                                 parent   = TaskList}, Todo2),

    State2 = updateTodoWindow(State),
    %% Make sure pasted task is selected after operation.
    wxListCtrl:setItemState(TodoList, Index,
                            ?wxLIST_STATE_SELECTED,
                            ?wxLIST_STATE_SELECTED),
    State2.


%%====================================================================
%% Redo
%%====================================================================
redoMenuEvent(_Type, _Id, _Frame, State) ->
    doRedo(State).

redoToolEvent(_Type, _Id, _Frame, State) ->
    doRedo(State).

doRedo(State) ->
    eTodoDB:redo(),
    State2 = updateTodoWindow(State),
    case State2 of
        #guiState{rows = ETodos, activeTodo = {_ETodo, ActiveIndex}} ->
            ETodo2 = getETodoAtIndex(ActiveIndex, ETodos),
            updateGui(ETodo2, ActiveIndex,
                      State2#guiState{activeTodo = {ETodo2, ActiveIndex}});
        State2 ->
            State2
    end.

%%====================================================================
%% Reply
%%====================================================================
replyMenuEvent(_Type, _Id, _Frame, State = #guiState{reply = []}) ->
    Obj = obj("msgTextCtrl",  State),
    wxTextCtrl:setFocus(Obj),
    State;
replyMenuEvent(_Type, _Id, _Frame, State = #guiState{reply = Reply}) ->
    Obj1 = obj("userCheckBox", State),
    Obj2 = obj("msgTextCtrl",  State),
    checkItemsInListWithClear(Obj1, [Reply]),
    wxTextCtrl:setFocus(Obj2),
    State.

replyAllMenuEvent(_Type, _Id, _Frame, State = #guiState{replyAll = []}) ->
    Obj = obj("msgTextCtrl",  State),
    wxTextCtrl:setFocus(Obj),
    State;
replyAllMenuEvent(_Type, _Id, _Frame, State = #guiState{reply    = Sender,
                                                        replyAll = ReplyAll}) ->
    Obj1 = obj("userCheckBox", State),
    Obj2 = obj("msgTextCtrl",  State),
    checkItemsInListWithClear(Obj1, [Sender|ReplyAll]),
    wxTextCtrl:setFocus(Obj2),
    State.

sendChatMenuEvent(Type, Id, Frame, State) ->
    sendChatMsgEvent(Type, Id, Frame, State).

%%====================================================================
%% Print
%%====================================================================
printMenuItemEvent(_Type, _Id, _Frame, State = #guiState{print     = Print,
                                                         searchCfg = Cfg,
                                                         filter    = Filter,
                                                         user      = User}) ->
    SearchText = wxComboBox:getValue(obj("searchText", State)),
    TaskList   = getTaskList(State),
    Filter2    = useFilter(TaskList, Filter, State),
    HtmlPage   = eHtml:printTaskList(User, TaskList, Filter2, SearchText, Cfg),
    %% Save copy of printout to disk.
    FileName = filename:join([getRootDir(), "www", "printout"]),
    filelib:ensure_dir(FileName),
    file:write_file(filename:join([getRootDir(), "www", "printout.html"]),
                    HtmlPage),
    wxHtmlEasyPrinting:previewText(Print, HtmlPage),
    State.

%%====================================================================
%% Set avatar
%%====================================================================
setAvatarMenuItemEvent(_Type, _Id, _Frame, State = #guiState{user = User}) ->
    case getAvatarFile(State) of
        {?wxID_OK, AbsFileName} ->
            Image   = wxImage:new(AbsFileName),
            Options = [{quality, ?wxIMAGE_QUALITY_HIGH}],
            Width   = wxImage:getWidth(Image),
            Height  = wxImage:getHeight(Image),
            Image3  = case Width > Height of
                          true ->
                              Y      = round(64/Width * Height),
                              PosY   = round((64 - Y) / 2),
                              Image2 = wxImage:scale(Image, 64, Y, Options),
                              wxImage:size(Image2, {64, 64}, {0, PosY}, []);
                          false ->
                              X      = round(64/Height * Width),
                              PosX   = round((64 - X) / 2),
                              Image2 = wxImage:scale(Image, X, 64, Options),
                              wxImage:size(Image2, {64, 64}, {PosX, 0}, [])
                      end,
            Bitmap = wxBitmap:new(Image3),
            CustomPortrait = getRootDir() ++
                "/Icons/portrait_" ++ User ++ ".png",
            wxBitmap:saveFile(Bitmap, CustomPortrait, ?wxBITMAP_TYPE_PNG),
            CustomPortrait2 = getRootDir() ++
                "/www/priv/Icons/portrait_" ++ User ++ ".png",
            wxBitmap:saveFile(Bitmap, CustomPortrait2, ?wxBITMAP_TYPE_PNG),
            userStatusUpdate(State),
            (catch setPortrait(User, State)),
            State;
        _ ->
            State
    end.

getAvatarFile(#guiState{frame = Frame}) ->
    WildCard = "BMP, JPEG, PNG and GIF files (*.bmp;*.gif;*.png;*.jpg)"
        "|*.bmp;*.gif;*.png;*.jpg",
    FileDlg = wxFileDialog:new(Frame, [{message, "Select avatar to use."},
                                       {wildCard, WildCard}]),
    Result = wxDialog:showModal(FileDlg),
    {Result, wxFileDialog:getPath(FileDlg)}.

%%====================================================================
%% Enter peer settings
%%====================================================================
settingsMenuEvent(_Type, _Id, _Frame,
                  State = #guiState{settingsDlg = Settings}) ->
    DefUserCfg = eTodoDB:readUserCfg(default),
    case DefUserCfg#userCfg.peerUser of
        undefined ->
            ok;
        PeerUser ->
            ConCfg = default(eTodoDB:getConnection(PeerUser), #conCfg{}),
            setDefaultValues(ConCfg, State)
    end,
    setDefaultValues(State),
    wxDialog:show(Settings),
    State.

webUIEnabledEvent(_Type, _Id, _Frame,
                  State = #guiState{settingsDlg = Settings}) ->
    WOnObj   = wxXmlResource:xrcctrl(Settings, "webUIEnabled",  wxCheckBox),
    WPwdObj  = wxXmlResource:xrcctrl(Settings, "webPassword",   wxTextCtrl),
    WPortObj = wxXmlResource:xrcctrl(Settings, "webPort",       wxTextCtrl),

    case wxCheckBox:isChecked(WOnObj) of
        true ->
            wxTextCtrl:enable(WPwdObj),
            wxTextCtrl:enable(WPortObj);
        false ->
            wxTextCtrl:disable(WPwdObj),
            wxTextCtrl:disable(WPortObj)
    end,
    State.

webProxyEnabledEvent(_Type, _Id, _Frame,
                     State = #guiState{settingsDlg = Settings}) ->
    WPOnObj   = wxXmlResource:xrcctrl(Settings, "webProxyEnabled",  wxCheckBox),
    WPHostObj = wxXmlResource:xrcctrl(Settings, "webProxyHost",     wxTextCtrl),
    WPPortObj = wxXmlResource:xrcctrl(Settings, "webProxyPort",     wxTextCtrl),

    case wxCheckBox:isChecked(WPOnObj) of
        true ->
            wxTextCtrl:enable(WPHostObj),
            wxTextCtrl:enable(WPPortObj);
        false ->
            wxTextCtrl:disable(WPHostObj),
            wxTextCtrl:disable(WPPortObj)
    end,
    State.

smtpEnabledEvent(_Type, _Id, _Frame,
    State = #guiState{settingsDlg = Settings}) ->
    SMTPOnObj   = wxXmlResource:xrcctrl(Settings, "smtpEnabled", wxCheckBox),
    SMTPSrvObj  = wxXmlResource:xrcctrl(Settings, "smtpServer",  wxTextCtrl),
    SMTPPortObj = wxXmlResource:xrcctrl(Settings, "smtpPort",    wxTextCtrl),
    SMTPUserObj = wxXmlResource:xrcctrl(Settings, "smtpUser",        wxTextCtrl),
    SMTPPwdObj  = wxXmlResource:xrcctrl(Settings, "smtpPwd",         wxTextCtrl),

    case wxCheckBox:isChecked(SMTPOnObj) of
        true ->
            wxTextCtrl:enable(SMTPSrvObj),
            wxTextCtrl:enable(SMTPUserObj),
            wxTextCtrl:enable(SMTPPwdObj),
            wxTextCtrl:enable(SMTPPortObj);
        false ->
            wxTextCtrl:disable(SMTPSrvObj),
            wxTextCtrl:disable(SMTPUserObj),
            wxTextCtrl:disable(SMTPPwdObj),
            wxTextCtrl:disable(SMTPPortObj)
    end,
    State.

msgFilterEvent(_Type, _Id, _Frame,
               State = #guiState{msgCfgDlg = Settings}) ->
    FilterObj  = wxXmlResource:xrcctrl(Settings, "msgFilter",      wxRadioBox),
    SChatObj   = wxXmlResource:xrcctrl(Settings, "showChat",       wxCheckBox),
    SAlarmObj  = wxXmlResource:xrcctrl(Settings, "showAlarm",      wxCheckBox),
    SSystemObj = wxXmlResource:xrcctrl(Settings, "showSystem",     wxCheckBox),
    AChoiceObj = wxXmlResource:xrcctrl(Settings, "msgAgentChoice", wxChoice),

    case wxRadioBox:getSelection(FilterObj) of
        0 ->
            wxCheckBox:enable(SChatObj),
            wxCheckBox:enable(SAlarmObj),
            wxCheckBox:enable(SSystemObj),
            wxChoice:disable(AChoiceObj);
        1 ->
            wxCheckBox:disable(SChatObj),
            wxCheckBox:disable(SAlarmObj),
            wxCheckBox:disable(SSystemObj),
            wxChoice:enable(AChoiceObj)
    end,
    State.

setDefaultValues(State = #guiState{user = User, settingsDlg = Settings}) ->
    WPwdObj     = wxXmlResource:xrcctrl(Settings, "webPassword",     wxTextCtrl),
    WPortObj    = wxXmlResource:xrcctrl(Settings, "webPort",         wxTextCtrl),
    EPortObj    = wxXmlResource:xrcctrl(Settings, "eTodoPort",       wxTextCtrl),
    WOnObj      = wxXmlResource:xrcctrl(Settings, "webUIEnabled",    wxCheckBox),
    WPOnObj     = wxXmlResource:xrcctrl(Settings, "webProxyEnabled", wxCheckBox),
    SMTPOnObj   = wxXmlResource:xrcctrl(Settings, "smtpEnabled",     wxCheckBox),
    SMTPSrvObj  = wxXmlResource:xrcctrl(Settings, "smtpServer",      wxTextCtrl),
    SMTPPortObj = wxXmlResource:xrcctrl(Settings, "smtpPort",        wxTextCtrl),
    WPHostObj   = wxXmlResource:xrcctrl(Settings, "webProxyHost",    wxTextCtrl),
    WPPortObj   = wxXmlResource:xrcctrl(Settings, "webProxyPort",    wxTextCtrl),
    EHostObj    = wxXmlResource:xrcctrl(Settings, "eTodoHostName",   wxTextCtrl),
    ThemeObj    = wxXmlResource:xrcctrl(Settings, "themeRadioBox",   wxRadioBox),
    SMTPUserObj = wxXmlResource:xrcctrl(Settings, "smtpUser",        wxTextCtrl),
    SMTPPwdObj  = wxXmlResource:xrcctrl(Settings, "smtpPwd",         wxTextCtrl),

    IPAddr   = eTodoUtils:getIp(),
    UConCfg  = default(eTodoDB:getConnection(User), #conCfg{}),

    wxTextCtrl:setValue(EHostObj, default(UConCfg#conCfg.host, IPAddr)),

    #userCfg{webEnabled      = WEnabled,
             webPort         = WPort,
             webPassword     = WPasswd,
             webProxyEnabled = WPEnabled,
             webProxyHost    = WPHost,
             webProxyPort    = WPPort,
             smtpEnabled     = SMTPOn,
             smtpServer      = SMTPServer,
             smtpPort        = SMTPPort,
             smtpUser        = SMTPUser,
             smtpPwd         = SMTPPwd,
             conPort         = EPort,
             theme           = Selected} = eTodoDB:readUserCfg(User),
    wxTextCtrl:setValue(SMTPPortObj,  toStr(default(SMTPPort, 25))),
    wxCheckBox:setValue(SMTPOnObj,    default(SMTPOn, false)),
    wxTextCtrl:setValue(SMTPSrvObj,   default(SMTPServer, "")),
    wxTextCtrl:setValue(SMTPPwdObj,   default(SMTPPwd, "")),
    wxTextCtrl:setValue(SMTPUserObj,  default(SMTPUser, "")),
    wxTextCtrl:setValue(WPortObj,     toStr(default(WPort,  8099))),
    wxTextCtrl:setValue(WPPortObj,    toStr(default(WPPort, 8099))),
    wxTextCtrl:setValue(EPortObj,     toStr(default(EPort,  19000))),
    wxTextCtrl:setValue(WPwdObj,      default(WPasswd, "")),
    wxTextCtrl:setValue(WPHostObj,    default(WPHost,  "")),
    wxCheckBox:setValue(WOnObj,       default(WEnabled,  false)),
    wxCheckBox:setValue(WPOnObj,      default(WPEnabled, false)),
    wxRadioBox:setSelection(ThemeObj, default(Selected, 0)),
    webUIEnabledEvent(undefined, undefined, undefined, State),
    smtpEnabledEvent(undefined, undefined, undefined, State),
    webProxyEnabledEvent(undefined, undefined, undefined, State).

setDefaultValues(ConCfg, #guiState{user = User, settingsDlg = Settings}) ->
    PeerObj  = wxXmlResource:xrcctrl(Settings, "peerUserName",  wxTextCtrl),
    HostObj  = wxXmlResource:xrcctrl(Settings, "hostName",      wxTextCtrl),
    PortObj  = wxXmlResource:xrcctrl(Settings, "peerPort",      wxTextCtrl),
    EHostObj = wxXmlResource:xrcctrl(Settings, "eTodoHostName", wxTextCtrl),

    UConCfg  = default(eTodoDB:getConnection(User), #conCfg{}),
    IPAddr   = eTodoUtils:getIp(),

    wxTextCtrl:setValue(EHostObj, default(UConCfg#conCfg.host,    IPAddr)),
    wxTextCtrl:setValue(PeerObj,  default(ConCfg#conCfg.userName, "")),
    wxTextCtrl:setValue(HostObj,  default(ConCfg#conCfg.host,     "")),
    wxTextCtrl:setValue(PortObj,  toStr(default(ConCfg#conCfg.port, 19000))).

settingsOkEvent(_Type, _Id, _Frame, State = #guiState{settingsDlg = Settings,
                                                      user        = User}) ->
    PeerObj     = wxXmlResource:xrcctrl(Settings, "peerUserName",    wxTextCtrl),
    HostObj     = wxXmlResource:xrcctrl(Settings, "hostName",        wxTextCtrl),
    PortObj     = wxXmlResource:xrcctrl(Settings, "peerPort",        wxTextCtrl),
    WPwdObj     = wxXmlResource:xrcctrl(Settings, "webPassword",     wxTextCtrl),
    WPortObj    = wxXmlResource:xrcctrl(Settings, "webPort",         wxTextCtrl),
    EPortObj    = wxXmlResource:xrcctrl(Settings, "eTodoPort",       wxTextCtrl),
    EHostObj    = wxXmlResource:xrcctrl(Settings, "eTodoHostName",   wxTextCtrl),
    WOnObj      = wxXmlResource:xrcctrl(Settings, "webUIEnabled",    wxCheckBox),
    ThemeObj    = wxXmlResource:xrcctrl(Settings, "themeRadioBox",   wxRadioBox),
    WPOnObj     = wxXmlResource:xrcctrl(Settings, "webProxyEnabled", wxCheckBox),
    WPHostObj   = wxXmlResource:xrcctrl(Settings, "webProxyHost",    wxTextCtrl),
    WPPortObj   = wxXmlResource:xrcctrl(Settings, "webProxyPort",    wxTextCtrl),
    SMTPOnObj   = wxXmlResource:xrcctrl(Settings, "smtpEnabled",     wxCheckBox),
    SMTPSrvObj  = wxXmlResource:xrcctrl(Settings, "smtpServer",      wxTextCtrl),
    SMTPPortObj = wxXmlResource:xrcctrl(Settings, "smtpPort",        wxTextCtrl),
    SMTPUserObj = wxXmlResource:xrcctrl(Settings, "smtpUser",        wxTextCtrl),
    SMTPPwdObj  = wxXmlResource:xrcctrl(Settings, "smtpPwd",         wxTextCtrl),

    FileName   = lists:concat(["styles_", User, ".css"]),
    DestFile   = filename:join([getRootDir(), "www", "priv", "css", FileName]),
    BlueTheme  = filename:join([getRootDir(),
                                "www", "priv", "css", "styles.css"]),
    BlackTheme = filename:join([getRootDir(),
                                "www", "priv", "css", "styles-black.css"]),

    Selection = case wxRadioBox:getSelection(ThemeObj) of
                    0 ->
                        file:copy(BlueTheme,  DestFile),
                        0;
                    1 ->
                        file:copy(BlackTheme, DestFile),
                        1
                end,

    PeerUser    = wxTextCtrl:getValue(PeerObj),
    PeerHost    = wxTextCtrl:getValue(HostObj),
    PeerPort    = wxTextCtrl:getValue(PortObj),

    WEnabled    = wxCheckBox:isChecked(WOnObj),
    WPwd        = wxTextCtrl:getValue(WPwdObj),
    WebPort     = wxTextCtrl:getValue(WPortObj),

    WPEnabled   = wxCheckBox:isChecked(WPOnObj),
    WPHost      = wxTextCtrl:getValue(WPHostObj),
    WPPort      = wxTextCtrl:getValue(WPPortObj),

    SMTPEnabled = wxCheckBox:isChecked(SMTPOnObj),
    SMTPServer  = wxTextCtrl:getValue(SMTPSrvObj),
    SMTPPort    = wxTextCtrl:getValue(SMTPPortObj),
    SMTPUser    = wxTextCtrl:getValue(SMTPUserObj),
    SMTPPwd     = wxTextCtrl:getValue(SMTPPwdObj),

    EPort       = wxTextCtrl:getValue(EPortObj),
    EHost       = wxTextCtrl:getValue(EHostObj),

    eTodoDB:updateConnection(#conCfg{userName   = PeerUser,
                                     host       = PeerHost,
                                     port       = toInt(PeerPort),
                                     updateTime = configured}),

    eTodoDB:updateConnection(#conCfg{userName   = User,
                                     host       = EHost,
                                     port       = toInt(EPort),
                                     updateTime = configured}),

    UserCfg  = eTodoDB:readUserCfg(User),
    UserCfg2 = UserCfg#userCfg{peerUser        = PeerUser,
                               webEnabled      = WEnabled,
                               webPassword     = WPwd,
                               webPort         = toInt(WebPort),
                               webProxyEnabled = WPEnabled,
                               webProxyHost    = WPHost,
                               webProxyPort    = toInt(WPPort),
                               smtpEnabled     = SMTPEnabled,
                               smtpServer      = SMTPServer,
                               smtpPort        = toInt(SMTPPort),
                               smtpUser        = SMTPUser,
                               smtpPwd         = SMTPPwd,
                               conPort         = toInt(EPort),
                               theme           = Selection},
    eTodoDB:saveUserCfg(UserCfg2),
    executeChanges(UserCfg, UserCfg2),
    wxDialog:hide(Settings),
    State.

toInt(Value) when is_list(Value) ->
    case catch list_to_integer(Value) of
        {'EXIT', _} ->
            19000;
        Integer ->
            Integer
    end.
executeChanges(#userCfg{webEnabled  = WEnabled,
                        webPassword = WPwd,
                        webPort     = WPort},
               #userCfg{webEnabled  = WEnabled,
                        webPassword = WPwd,
                        webPort     = WPort}) ->
    %% No changes, done.
    ok;
executeChanges(#userCfg{webEnabled = false},
               UserCfg = #userCfg{webEnabled = true}) ->
    {ok, Pid} = eWeb:start_link(UserCfg#userCfg.userName),
    monitor(process, Pid),
    {ok, Pid};
executeChanges(_, UserCfg) ->
    eWeb:stop(),
    timer:sleep(1000),
    {ok, Pid} = eWeb:start_link(UserCfg#userCfg.userName),
    monitor(process, Pid),
    {ok, Pid}.

settingsCancelEvent(_Type, _Id, _Frame,
                    State = #guiState{settingsDlg = Settings}) ->
    wxDialog:hide(Settings),
    State.

msgSettingsButtonEvent(_Type, _Id, _Frame,
                       State = #guiState{msgCfgDlg = Settings,
                                         msgCfg    = {Flt, {C, A, S}, UserName},
                                         user      = User}) ->

    Alarm  = wxXmlResource:xrcctrl(Settings, "showAlarm",  wxCheckBox),
    Chat   = wxXmlResource:xrcctrl(Settings, "showChat",   wxCheckBox),
    System = wxXmlResource:xrcctrl(Settings, "showSystem", wxCheckBox),

    FilterType = wxXmlResource:xrcctrl(Settings, "msgFilter",      wxRadioBox),
    UserChoice = wxXmlResource:xrcctrl(Settings, "msgAgentChoice", wxChoice),

    case Flt of
        type ->
            wxRadioBox:setSelection(FilterType, 0);
        user ->
            wxRadioBox:setSelection(FilterType, 1)
    end,

    wxCheckBox:setValue(Chat,   C),
    wxCheckBox:setValue(Alarm,  A),
    wxCheckBox:setValue(System, S),

    Users  = lists:delete(User, eTodoDB:getUsers()),
    Choice = wxXmlResource:xrcctrl(Settings, "msgAgentChoice", wxChoice),
    wxChoice:clear(Choice),
    wxChoice:appendStrings(Choice, Users),
    wxChoice:setStringSelection(UserChoice, UserName),

    wxDialog:setSize(Settings, {270, 325}),
    wxDialog:show(Settings),
    msgFilterEvent(undefined, undefined, undefined, State).

msgSettingsCancelEvent(_Type, _Id, _Frame,
                       State = #guiState{msgCfgDlg = Settings}) ->
    wxDialog:hide(Settings),
    State.

msgSettingsOkEvent(_Type, _Id, _Frame,
                   State  = #guiState{msgCfgDlg = Settings, user = User}) ->
    Chat    = wxXmlResource:xrcctrl(Settings, "showChat",   wxCheckBox),
    Alarm   = wxXmlResource:xrcctrl(Settings, "showAlarm",  wxCheckBox),
    System  = wxXmlResource:xrcctrl(Settings, "showSystem", wxCheckBox),
    ChatB   = wxCheckBox:isChecked(Chat),
    AlarmB  = wxCheckBox:isChecked(Alarm),
    SysB    = wxCheckBox:isChecked(System),

    FilterType = wxXmlResource:xrcctrl(Settings, "msgFilter",      wxRadioBox),
    UserChoice = wxXmlResource:xrcctrl(Settings, "msgAgentChoice", wxChoice),
    UserName   = wxChoice:getStringSelection(UserChoice),

    State2 = case wxRadioBox:getSelection(FilterType) of
                 0 ->
                     Cfg = {type, {ChatB, AlarmB, SysB}, UserName},
                     State#guiState{msgCfg = Cfg};
                 1 ->
                     Cfg = {user, {ChatB, AlarmB, SysB}, UserName},
                     State#guiState{msgCfg = Cfg}
             end,
    wxDialog:hide(Settings),
    updateMsgWindow(State2, User).

bookmarkBtnEvent(context_menu, _Id, _Frame,
                 State = #guiState{bookmCfg = BookmCfg}) ->
    Items = [Bookmark || {Bookmark, _Cfg} <- BookmCfg],
    case length(Items) > 0 of
        true ->
            showBookmarkMenu(Items, State);
        false ->
            State
    end;
bookmarkBtnEvent(_Type, _Id, _Frame, State = #guiState{manBookmDlg = Manage,
                                                       bookmCfg    = BookmCfg}) ->
    Obj = wxXmlResource:xrcctrl(Manage, "manageBookmarkBox", wxListBox),
    wxListBox:clear(Obj),
    [wxListBox:append(Obj, Bookmark) || {Bookmark, _Cfg} <- BookmCfg],
    wxDialog:setSize(Manage, {350, 350}),
    wxDialog:show(Manage),
    State.

%%====================================================================
%% Plugin events
%%====================================================================
pluginMenuItemEvent(_Type, _Id, _Frame, State = #guiState{pluginDlg = Plugins}) ->
    PluginList = ePluginServer:getInstalledPlugins(),
    Obj = wxXmlResource:xrcctrl(Plugins, "usePlugins", wxCheckListBox),
    wxCheckListBox:clear(Obj),
    [wxCheckListBox:append(Obj, Name) || {_, Name, _} <- PluginList],
    NList = [Plugin:getName() || Plugin <- ePluginServer:getConfiguredPlugins()],
    checkItemsInList(Obj, NList),
    wxDialog:show(Plugins),
    State.

pluginOkEvent(_Type, _Id, _Frame, State = #guiState{pluginDlg = Plugins}) ->
    wxDialog:hide(Plugins),
    Obj = wxXmlResource:xrcctrl(Plugins, "usePlugins", wxCheckListBox),
    PluginList      = ePluginServer:getInstalledPlugins(),
    PluginNameList  = getCheckedItems(Obj),
    PluginFNameList = [getFileName(Name, PluginList) || Name <- PluginNameList],
    ePluginServer:setConfiguredPlugins(PluginFNameList),
    State.

pluginCancelEvent(_Type, _Id, _Frame, State = #guiState{pluginDlg = Plugins}) ->
    wxDialog:hide(Plugins),
    State.

usePluginsEvent(_Type, _Id, _Frame, State = #guiState{pluginDlg = Plugins}) ->
    Obj1 = wxXmlResource:xrcctrl(Plugins, "usePlugins", wxCheckListBox),
    Obj2 = wxXmlResource:xrcctrl(Plugins, "pluginDesc", wxStaticText),
    Selected     = wxCheckListBox:getSelection(Obj1),
    Name         = wxCheckListBox:getString(Obj1, Selected),
    PluginList   = ePluginServer:getInstalledPlugins(),
    {_, _, Desc} = lists:keyfind(Name, 2, PluginList),
    wxStaticText:setLabel(Obj2, Desc),
    wxStaticText:wrap(Obj2, 180),
    State.

getFileName(Name, PluginList) ->
    {FName, Name, _Desc} = lists:keyfind(Name, 2, PluginList),
    FName.

%%====================================================================
%% Toolbar
%%====================================================================
configureSearchEvent(_Type, _Id, _Frame, State = #guiState{searchDlg = Search,
                                                           searchCfg = Cfg}) ->
    (catch setCfgColumns(Search, Cfg)),
    wxDialog:show(Search),
    State.

setCfgColumns(Search, Cfg) ->
    Obj = wxXmlResource:xrcctrl(Search, "searchColumns",  wxCheckListBox),
    Ext = [taskExternal(Col) || Col <- Cfg],
    checkItemsInListWithClear(Obj, Ext).

getCfgColumns(Search) ->
    Obj = wxXmlResource:xrcctrl(Search, "searchColumns",  wxCheckListBox),
    Ext = getCheckedItems(Obj),
    [taskInternal(Col) || Col <- Ext].

searchOkEvent(_Type, _Id, _Frame, State = #guiState{searchDlg = Search}) ->
    wxDialog:hide(Search),
    State#guiState{searchCfg = getCfgColumns(Search)}.

searchCancelEvent(_Type, _Id, _Frame, State = #guiState{searchDlg = Search}) ->
    wxDialog:hide(Search),
    State.

searchTextEvent(_Type, _Id, _Frame, State) ->
    SearchCB   = obj("searchText", State),
    SearchText = wxComboBox:getValue(SearchCB),
    case valueNotPresent(SearchCB, SearchText) of
        true ->
            wxComboBox:insert(SearchCB, SearchText, 0),
            case wxComboBox:getCount(SearchCB) > 15 of
                true ->
                    wxComboBox:delete(SearchCB, 15);
                false ->
                    ok
            end;
        false ->
            ok
    end,
    updateTodoWindow(State).

valueNotPresent(SearchCB, SearchText) ->
    Count = wxComboBox:getCount(SearchCB),
    valueNotPresent(SearchCB, SearchText, 0, Count).

valueNotPresent(_SearchCB, "", _Num, _Count) ->
    false; %% Do not add empty searches to combobox.
valueNotPresent(_SearchCB, _SearchText, Count, Count) ->
    true;
valueNotPresent(SearchCB, SearchText, Num, Count) ->
    case wxComboBox:getString(SearchCB, Num) of
        SearchText ->
            false;
        _ ->
            valueNotPresent(SearchCB, SearchText, Num + 1, Count)
    end.

%%====================================================================
%% Undo
%%====================================================================
undoMenuEvent(_Type, _Id, _Frame, State) ->
    checkUndoStatus(doUndo(State)).

undoToolEvent(_Type, _Id, _Frame, State) ->
    checkUndoStatus(doUndo(State)).

doUndo(State) ->
    eTodoDB:undo(),
    State2 = updateTodoWindow(State),
    case State2 of
        #guiState{rows = ETodos, activeTodo = {_ETodo, Index}} ->
            ETodo2 = getETodoAtIndex(Index, ETodos),
            updateGui(ETodo2, Index,
                      State2#guiState{activeTodo = {ETodo2, Index}});
        State2 ->
            State2
    end.

%%====================================================================
%% Gui event: unnamed events are received here.
%%====================================================================
confirmRemove(State, Frame, User, Type, GuiDesc) ->
    Dlg = wxMessageDialog:new(Frame, "Are you sure you want to remove " ++
                                  GuiDesc ++ "?",
                              [{caption, "Remove messages"},
                               {style,   ?wxYES_NO}]),
    case wxMessageDialog:showModal(Dlg) of
        ?wxID_YES ->
            eTodoDB:delMessages(User, Type),
            wxMessageDialog:destroy(Dlg),
            updateMsgWindow(State, User);
        _ ->
            wxMessageDialog:destroy(Dlg),
            State
    end.

confirmRemoveLinked(State, Frame) ->
    Dlg = wxMessageDialog:new(Frame, "Are you sure you want to remove "
                              "linked files?",
                              [{caption, "Remove linked files"},
                               {style,   ?wxYES_NO}]),
    case wxMessageDialog:showModal(Dlg) of
        ?wxID_YES ->
            FileDir = filename:join([getRootDir(), "www", "linkedFiles"]),
            {ok, FileList} = file:list_dir(FileDir),
            [file:delete(filename:join([getRootDir(),
                                        "www", "linkedFiles", File])) ||
                File <- FileList],
            State;
        _ ->
            State
    end.

guiEvent(_Type, MenuOption, _Frame,
         State = #guiState{activeTodo = {ETodo, _}, user = User})
  when MenuOption >= ?plugins ->
    ePluginServer:eMenuEvent(User, MenuOption, ETodo),
    State;
guiEvent(_Type, ?clearMsg, Frame, State = #guiState{user = User}) ->
    confirmRemove(State, Frame, User, msgEntry, "chat messages");
guiEvent(_Type, ?clearSys, Frame, State = #guiState{user = User}) ->
    confirmRemove(State, Frame, User, systemEntry, "system messages");
guiEvent(_Type, ?clearRem, Frame, State = #guiState{user = User}) ->
    confirmRemove(State, Frame, User, alarmEntry, "reminders");
guiEvent(_Type, ?clearLinked, Frame, State) ->
    confirmRemoveLinked(State, Frame);

guiEvent(_Type, ?sortDef, _Frame, State = #guiState{user      = User}) ->
    eTodoDB:saveListCfg(User, sorted, default),
    updateTodoWindow(State);

guiEvent(_Type, ?sortAsc, _Frame, State = #guiState{popUpCol  = ColName,
                                                    user      = User}) ->

    eTodoDB:saveListCfg(User, sorted, {ascending, ColName}),
    updateTodoWindow(State);
guiEvent(_Type, ?sortDec, _Frame, State = #guiState{popUpCol  = ColName,
                                                    user      = User}) ->
    eTodoDB:saveListCfg(User, sorted, {descending, ColName}),
    updateTodoWindow(State);
guiEvent(_Type, Lists, _Frame, State = #guiState{user       = User,
                                                 activeTodo = {ETodo, Index}})
  when (Lists > ?lists) and (Lists < ?bookmarks) ->
    TodoLists = getTodoLists(User),
    List      = lists:nth(Lists - ?lists, TodoLists),
    OldLists  = ETodo#etodo.listsDB,
    NewLists =
        case {List, lists:member(List, OldLists)} of
            {?defTaskList, _} ->
                %% You cannot remove All Tasks list
                OldLists;
            {_, true} ->
                %% Remove list since it was already in list
                OldLists -- [List];
            {_, false} ->
                %% Add to list since it wasn't in the list
                lists:sort([List|OldLists])
        end,

    eTodoDB:assignLists(User, ETodo#etodo.uidDB, NewLists),

    ListsObj = obj("addedToLists", State),
    wxStaticText:setLabel(ListsObj, makeStr(NewLists)),

    ETodo2 = ETodo#etodo{lists = makeStr(NewLists), listsDB = NewLists},

    TaskList = getTaskList(State),
    case lists:member(TaskList, NewLists) of
        true ->
            State2 = State#guiState{activeTodo = {ETodo2, Index}},
            Rows2  = updateRows(ETodo2, State2#guiState.rows),
            State2#guiState{rows = Rows2};
        false ->
            State2 = updateTodoWindow(State),
            focusAndSelect(State2#guiState{activeTodo = {undefined, -1}})
    end;
guiEvent(_Type, Column, _Frame, State = #guiState{user = User})
  when (Column >= ?showcolumns) and (Column < ?sortColumns) ->
    saveColumnSizes(State),
    ColumnsInfo = eTodoDB:getColumns(User),
    Columns     = [Desc || {_Col, Desc} <- ColumnsInfo],
    ColumnDesc  = lists:nth(Column - ?showcolumns, Columns),
    ColumnInt   = taskInternal(ColumnDesc),
    ColumnSett  = default(eTodoDB:readListCfg(User, ColumnInt, visible), true),
    eTodoDB:saveListCfg(User, ColumnInt, visible, not ColumnSett),

    TaskList    = getTaskList(State),
    TodoList    = getTodoList(TaskList, State),
    [setColumnWidth(TodoList, User, Col, Desc) || {Col, Desc} <- ColumnsInfo],
    State;
guiEvent(_Type, ?sortColumns, _Frame, State = #guiState{sortColsDlg = SortCols,
                                                        user        = User}) ->
    ColumnsInfo = eTodoDB:getColumns(User),
    Obj = wxXmlResource:xrcctrl(SortCols, "sortColumnsBox", wxListBox),
    wxListBox:clear(Obj),
    [wxListBox:insert(Obj, Desc, Col) || {Col, Desc} <- ColumnsInfo],
    wxDialog:setSize(SortCols, {350, 350}),
    wxDialog:show(SortCols),
    State;
guiEvent(_Type, Bookmark, _Frame, State = #guiState{bookmCfg = BookmCfg})
  when Bookmark > ?bookmarks ->
    case lists:nth(Bookmark - ?bookmarks, BookmCfg) of
        {_, {TaskList, Filter, SearchCfg, SearchText}} ->
            wxComboBox:setValue(obj("searchText", State), SearchText),
            setSelection(obj("taskListChoice", State), TaskList),
            State2 = updateTodoWindow(State#guiState{filter    = Filter,
                                                     searchCfg = SearchCfg}),
            focusAndSelect(State2#guiState{activeTodo = {undefined, -1}});
        _ ->
            State
    end;
guiEvent(_Type, Status, _Frame,
         State = #guiState{popUpCol  = {row, Row}, user = User}) ->
    ETodo  = getETodoAtIndex(Row, State#guiState.rows),
    ETodo2 = ETodo#etodo{statusDB = toStatusDB(Status),
                         status   = toStr(toStatusDB(Status))},
    updateTodoInDB(User, ETodo2),
    TaskList = getTaskList(State),
    TodoList = getTodoList(TaskList, State),
    State2 = updateTodo(TodoList, ETodo2, Row, State),
    updateGui(ETodo, ETodo2, Row, State2);
guiEvent(_Type, FilterKey, _Frame, State = #guiState{filter  = Filter}) ->
    Filter2 = updateFilter(FilterKey, Filter),
    updateTodoWindow(State#guiState{filter = Filter2}).

%% Adds to filter if not present, removes if present.
updateFilter(Key , Filter) ->
    updateFilter(Key, Filter, []).

updateFilter(Key, [], SoFar)             -> [Key|SoFar];
updateFilter(Key, [Key | Filter], SoFar) -> Filter ++ SoFar;
updateFilter(Key, [Key2 | Rest],  SoFar) ->
    updateFilter(Key, Rest, [Key2 | SoFar]).

descNotebookEvent(_Type, _Id, _Frame, State) ->
    Notebook     = obj("descNotebook", State),
    CurrPage     = wxNotebook:getCurrentPage(Notebook),
    PreviewPage  = wxNotebook:getPage(Notebook, 1),
    case CurrPage of
        PreviewPage ->
            Obj      = obj("descAreaPreview", State),
            DescObj  = obj("descriptionArea", State),
            Descript = wxTextCtrl:getValue(DescObj),
            Page     = eMd2Html:convert(Descript),
            wxHtmlWindow:setPage(Obj, unicode:characters_to_list(Page, utf8));
        _ ->
            ok
    end,
    State.

commentNotebookEvent(_Type, _Id, _Frame, State) ->
    Notebook     = obj("commentNotebook", State),
    CurrPage     = wxNotebook:getCurrentPage(Notebook),
    PreviewPage  = wxNotebook:getPage(Notebook, 1),
    case CurrPage of
        PreviewPage ->
            Obj        = obj("commentAreaPreview", State),
            CommentObj = obj("commentArea",     State),
            Comment    = wxTextCtrl:getValue(CommentObj),
            Page       = eMd2Html:convert(Comment),
            wxHtmlWindow:setPage(Obj, unicode:characters_to_list(Page, utf8));
        _ ->
            ok
    end,
    State.

msgNotebookEvent(_Type, _Id, _Frame, State) ->
    Notebook     = obj("msgNotebook", State),
    CurrPage     = wxNotebook:getCurrentPage(Notebook),
    PreviewPage  = wxNotebook:getPage(Notebook, 1),
    case CurrPage of
        PreviewPage ->
            Obj        = obj("msgTextPreview", State),
            CommentObj = obj("msgTextCtrl",    State),
            Comment    = wxTextCtrl:getValue(CommentObj),
            Page       = eMd2Html:convert(Comment),
            wxHtmlWindow:setPage(Obj, unicode:characters_to_list(Page, utf8));
        _ ->
            ok
    end,
    State.

mainNotebookEvent(_Type, _Id, _Frame, State) ->
    Notebook     = obj("mainNotebook",  State),
    CurrPage     = wxNotebook:getCurrentPage(Notebook),
    MsgPage      = wxNotebook:getPage(Notebook, 1),
    WorkReport   = wxNotebook:getPage(Notebook, 2),
    TimeReport   = wxNotebook:getPage(Notebook, 3),
    ScheduleRep  = wxNotebook:getPage(Notebook, 4),

    case CurrPage of
        MsgPage ->
            State2 = clearMsgCounter(State),
            clearStatusBar(State2);
        WorkReport ->
            eGuiFunctions:generateWorkLog(State);
        TimeReport ->
            eGuiFunctions:generateTimeLog(State);
        ScheduleRep ->
            eGuiFunctions:generateSchedule(State);
        _ ->
            State
    end.


%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getItems(Obj) ->
    Count = wxListBox:getCount(Obj),
    getItems(Obj, Count - 1, []).

getItems(_Obj, -1, Items) ->
    Items;
getItems(Obj, Index, Items) ->
    Item = listBoxGet(Obj, Index),
    getItems(Obj, Index - 1, [Item|Items]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
checkItemsInList(Obj, List) ->
    Count = wxCheckListBox:getCount(Obj),
    checkItemsInList(Obj, Count - 1, List).

checkItemsInList(_Obj, -1, _List) ->
    ok;
checkItemsInList(Obj, Index, List) ->
    Item = wxCheckListBox:getString(Obj, Index),
    case lists:member(Item, List) of
        true ->
            wxCheckListBox:check(Obj, Index);
        false ->
            ok
    end,
    checkItemsInList(Obj, Index - 1, List).

checkItemsInListWithClear(Obj, List) ->
    Count = wxCheckListBox:getCount(Obj),
    checkItemsInListWithClear(Obj, Count - 1, List).

checkItemsInListWithClear(_Obj, -1, _List) ->
    ok;
checkItemsInListWithClear(Obj, Index, List) ->
    Item = wxCheckListBox:getString(Obj, Index),
    case lists:member(Item, List) of
        true ->
            wxCheckListBox:check(Obj, Index);
        false ->
            wxCheckListBox:check(Obj, Index, [{check, false}])
    end,
    checkItemsInListWithClear(Obj, Index - 1, List).


listBoxGet(Obj, Index) ->
    Count = wxListBox:getCount(Obj),
    case Index < Count of
        true ->
            wxListBox:getString(Obj, Index);
        false ->
            ""
    end.

constructTooltip(LTime) ->
    constructTooltip(LTime, []).

constructTooltip([], Acc) ->
    lists:flatten(lists:sublist(Acc, 1, 49));
constructTooltip([Time], Acc) ->
    constructTooltip([], [Time|Acc]);
constructTooltip([Time|Rest], Acc) ->
    constructTooltip(Rest, ["\n", Time|Acc]).