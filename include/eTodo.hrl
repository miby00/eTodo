%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2012 by mikael <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

%%====================================================================
%% guiState for eTodo.
%%====================================================================
-record(guiState, {dict, user, loggedIn = false, frame, updList,
                   searchDlg, searchCfg = [description, comment],
                   loginDlg, timeDlg, activeTodo = {undefined, -1},
                   clipBoard, startup, columns, rows = [], popUpMenu,
                   popUpCol, filter = [], usersDlg, addListDlg, manListsDlg,
                   settingsDlg, drillDown = [], drillFromList, mode,
                   print, conflictDlg, msgMenu, msgStatusSent = false,
                   aboutDlg, manOwnerDlg, helpFrame, userStatus = [],
                   unreadMsgs = 0, unreadSysMsgs = 0, delayedUpdate,
                   reply = [], replyAll = [], toolBar, menuBar,
                   manBookmDlg, popupBookmMenu, bookmCfg = [],
                   sortColsDlg, timerDlg, timerRef, pluginDlg, logWorkDlg,
                   timerDlgOpen = false}).

-define(wxTypes, #{
        "aboutMenu"          => wxMenuItem,
        "addedToLists"       => wxStaticText,
        "addListButton"      => wxBitmapButton,
        "addOwnerButton"     => wxBitmapButton,
        "addTaskInboxMenu"   => wxMenuItem,
        "addTaskMenu"        => wxMenuItem,
        "allMsgPanel"        => wxPanel,
        "backupMenuItem"     => wxMenuItem,
        "bookmarkBtn"        => wxBitmapButton,
        "checkBoxUseFilter"  => wxCheckBox,
        "chooseListLang"     => wxStaticText,
        "commentArea"        => wxTextCtrl,
        "commentArea1"       => wxTextCtrl,
        "commentArea2"       => wxTextCtrl,
        "commentButton"      => wxBitmapButton,
        "commentTextPanel"   => wxPanel,
        "commentMdPanel"     => wxPanel,
        "commentNotebook"    => wxNotebook,
        "commentAreaPreview" => wxHtmlWindow,
        "completeLang"       => wxStaticText,
        "configureSearch"    => wxBitmapButton,
        "copyMenu"           => wxMenuItem,
        "cutMenu"            => wxMenuItem,
        "deleteMenu"         => wxMenuItem,
        "descNotebook"       => wxNotebook,
        "descAreaPreview"    => wxHtmlWindow,
        "descMdPanel"        => wxPanel,
        "descTextPanel"      => wxPanel,
        "descriptionArea"    => wxTextCtrl,
        "descriptionArea1"   => wxTextCtrl,
        "descriptionArea2"   => wxTextCtrl,
        "dueDateLang"        => wxStaticText,
        "dueDatePicker"      => wxDatePickerCtrl,
        "dueDatePicker1"     => wxDatePickerCtrl,
        "dueDatePicker2"     => wxDatePickerCtrl,
        "dueDateUsed"        => wxCheckBox,
        "editMenu"           => wxMenu,
        "endDate"            => wxDatePickerCtrl,
        "eTodoToolbar"       => wxToolBar,
        "exitMenuItem"       => wxMenuItem,
        "fileMenu"           => wxMenu,
        "helpMenu"           => wxMenu,
        "helpMenu1"          => wxMenuItem,
        "infoIcon"           => wxStaticBitmap,
        "linkFileMenu"       => wxMenuItem,
        "linkTimeReportMenu" => wxMenuItem,
        "linkViewMenu"       => wxMenuItem,
        "listCheckBox"       => wxCheckListBox,
        "listsLang"          => wxStaticText,
        "loginMenuItem"      => wxMenuItem,
        "logoutMenuItem"     => wxMenuItem,
        "logWorkButton"      => wxBitmapButton,
        "mainNotebook"       => wxNotebook,
        "mainPanel"          => wxPanel,
        "mainStatusBar"      => wxStatusBar,
        "mainTaskList"       => wxListCtrl,
        "manageListsButton"  => wxBitmapButton,
        "messagePanel"       => wxPanel,
        "moveDownMenu"       => wxMenuItem,
        "moveFirstMenu"      => wxMenuItem,
        "moveLastMenu"       => wxMenuItem,
        "moveUpMenu"         => wxMenuItem,
        "msgTextCtrl"        => wxTextCtrl,
        "msgTextWin"         => wxHtmlWindow,
        "ownerChoice"        => wxChoice,
        "ownerChoice1"       => wxChoice,
        "ownerChoice2"       => wxChoice,
        "ownerLang"          => wxStaticText,
        "pasteMenu"          => wxMenuItem,
        "peerAvailableIcon"  => wxStaticBitmap,
        "peerAwayIcon"       => wxStaticBitmap,
        "peerBusyIcon"       => wxStaticBitmap,
        "peerStatusMsg"      => wxStaticText,
        "pluginMenuItem"     => wxMenuItem,
        "portraitPeerIcon"   => wxStaticBitmap,
        "portraitUserIcon"   => wxStaticBitmap,
        "printMenuItem"      => wxMenuItem,
        "priorityChoice"     => wxChoice,
        "priorityChoice1"    => wxChoice,
        "priorityChoice2"    => wxChoice,
        "priorityLang"       => wxStaticText,
        "progressInfo"       => wxSpinCtrl,
        "progressInfo1"      => wxSpinCtrl,
        "progressInfo2"      => wxSpinCtrl,
        "proxyLinkMenu"      => wxMenuItem,
        "recurrence"         => wxRadioBox,
        "themeRadioBox"      => wxRadioBox,
        "redoMenu"           => wxMenuItem,
        "reminderPanel"      => wxPanel,
        "remTextWin"         => wxHtmlWindow,
        "replyAllMenu"       => wxMenuItem,
        "replyMenu"          => wxMenuItem,
        "restoreMenuItem"    => wxMenuItem,
        "schedulePanel"      => wxPanel,
        "scheduleReport"     => wxHtmlWindow,
        "searchText"         => wxComboBox,
        "sendTaskButton"     => wxBitmapButton,
        "setAvatarMenuItem"  => wxMenuItem,
        "setReminderButton"  => wxBitmapButton,
        "settingsMenu"       => wxMenuItem,
        "shareButton"        => wxBitmapButton,
        "shareButton2"       => wxBitmapButton,
        "sharedLang"         => wxStaticText,
        "sharedWithText"     => wxStaticText,
        "sharedWithText1"    => wxTextCtrl,
        "sharedWithText2"    => wxTextCtrl,
        "splitter"           => wxSplitterWindow,
        "splitterMain"       => wxSplitterWindow,
        "splitterComment"    => wxSplitterWindow,
        "splitterMsg"        => wxSplitterWindow,
        "startDate"          => wxDatePickerCtrl,
        "statusChoice"       => wxChoice,
        "statusChoice1"      => wxChoice,
        "statusChoice2"      => wxChoice,
        "statusLang"         => wxStaticText,
        "taskEditPanel"      => wxPanel,
        "taskListChoice"     => wxChoice,
        "timeLogPanel"       => wxPanel,
        "timeLogReport"      => wxHtmlWindow,
        "undoMenu"           => wxMenuItem,
        "useEndDate"         => wxCheckBox,
        "userAvailableIcon"  => wxStaticBitmap,
        "userAwayIcon"       => wxStaticBitmap,
        "userBusyIcon"       => wxStaticBitmap,
        "userCheckBox"       => wxCheckListBox,
        "userOfflineIcon"    => wxStaticBitmap,
        "userStatusChoice"   => wxChoice,
        "userStatusMsg"      => wxComboBox,
        "userStatusPanel"    => wxPanel,
        "useStartDate"       => wxCheckBox,
        "workLogPanel"       => wxPanel,
        "workLogReport"      => wxHtmlWindow,
        "workLogStartDate"   => wxDatePickerCtrl
       }).

-include_lib("eTodo/include/eLang.hrl").

%%====================================================================
%% Table records for mnesia
%%====================================================================
-record(todo, {uid,
               priority,
               status,
               doneTime,
               dueTime,
               createTime,
               description,
               comment,
               progress,
               sharedWith,
               owner}).

-record(userInfo, {userName,
                   uid,
                   parent,
                   row}).

-record(userCfg,  {userName,
                   windowSize,
                   lastCircle,
                   lastUser,
                   peerUser,
                   lists,
                   webEnabled = false,
                   webPassword,
                   webPort,
                   conPort,
                   ownerCfg,
                   showLogin = false,
                   oldPwd,
                   splitterMain    = 250,
                   splitterComment = 400,
                   splitterMsg     = 100,
                   filter          = [],
                   bookmCfg        = [],
                   plugins         = [],
                   theme           = 0,
                   lastTaskList,
                   webSettings     = [],
                   unreadMsgs      = [],
                   lastTimer       = {0,25,0}}).

-record(listCfg,  {key,
                   userName,
                   columnName,
                   columnWidth,
                   sorted,
                   order,
                   visible = true}).

-record(alarmCfg, {uid,
                   userName,
                   startDate,
                   startTime,
                   endDate,
                   recurrence,
                   execCmd,
                   lastAlarm,
                   nextAlarm}).

-record(conCfg,    {userName, host, port, owner, distance, updateTime}).

-record(logWork,   {userName, uid, date, hours, minutes}).

-record(workDesc,  {uid, shortDesc = "",
                    showInWorkLog = false,
                    showInTimeLog = false}).

-record(logTime,   {uid, timeEstimate = 0, timeRemaining = 0}).

%%====================================================================
%% Diff record used to send updates to other nodes.
%%====================================================================
-record(diff, {uid, diff}).

%%====================================================================
%% User information, to be used in the message window.
%%====================================================================
-record(userStatus, {userName = "", statusMsg = "", status = "Available"}).

%%====================================================================
%% Record with all the columns visible in the gui plus the internal uid.
%% Used by eTodo and eTodoDB.
%%====================================================================
-record(etodo,  {status,
                 statusCol,
                 statusDB,
                 priority,
                 priorityCol,
                 priorityDB,
                 owner,
                 ownerCol,
                 dueTime,
                 dueTimeCol,
                 dueTimeDB,
                 description,
                 descriptionCol,
                 comment,
                 commentCol,
                 sharedWith,
                 sharedWithCol,
                 sharedWithDB,
                 createTime,
                 createTimeCol,
                 createTimeDB,
                 doneTime,
                 doneTimeCol,
                 doneTimeDB,
                 hasSubTodo,
                 uid,
                 uidCol,
                 uidDB,
                 progress,
                 lists,
                 listsDB}).

%% Constants for pop up menu.
-define(sortDef,          1001).
-define(sortAsc,          1002).
-define(sortDec,          1003).

-define(statusPlanning,   1010).
-define(statusInProgress, 1011).
-define(statusDone,       1012).
-define(statusNone,       1013).

-define(prioLow,          1020).
-define(prioMedium,       1021).
-define(prioHigh,         1022).
-define(prioNone,         1023).

-define(useLocal,         1050).
-define(useRemote,        1051).

-define(clearMsg,         1060).
-define(clearRem,         1061).
-define(clearLinked,      1062).

-define(assigned,         1065).

-define(about,            1070).

-define(showcolumns,      1075).

-define(sortColumns,      1099).

-define(lists,            1100).

-define(bookmarks,        1200).

-define(plugins,          1300).

%% Constants for gui
-define(clearMsgText,       ?tr(clearMsgText)).
-define(clearRemText,       ?tr(clearRemText)).
-define(clearLinkedText,    ?tr(clearLinkedText)).

-define(clearMsgText(LANG),    ?tr(LANG, clearMsgText)).
-define(clearRemText(LANG),    ?tr(LANG, clearRemText)).
-define(clearLinkedText(LANG), ?tr(LANG, clearLinkedText)).

-define(descSortDef,    "Sort order default").
-define(descSortAsc,    "Sort order ascending").
-define(descSortDec,    "Sort order descending").

-define(descNone,       "").
-define(descNA,         "N/A").

-define(descPlanning,   "Planning").
-define(descInProgress, "In progress").
-define(descDone,       "Done").

-define(descAll,        "All").

-define(descLow,        "Low").
-define(descMedium,     "Medium").
-define(descHigh,       "High").

-define(descDef,        "Default").
-define(descCompact,    "Compact").
-define(descAssigned,   "Assigned").

-define(uid,            "Id").
-define(status,         "Status").
-define(prio,           "Priority").
-define(dueTime,        "Due date").
-define(description,    "Description").
-define(comment,        "Comment").
-define(sharedWith,     "Shared with").
-define(createTime,     "Create time").
-define(doneTimestamp,  "Done time").
-define(owner,          "Owner").

-define(estimate,       "Estimate(h)").
-define(remaining,      "Remaining(h)").
-define(progress,       "Progress(%)").

-define(sbNotLoggedIn,  "Not logged in").

-define(defTaskList,    "All tasks").

-define(defInbox,       "Inbox").
-define(defLoggedWork,  "-- Logged work --").
-define(defTimeReport,  "-- Time report --").
-define(defShowStatus,  "-- Show status --").
-define(defSchedule,    "---- Schedule ----").

-define(subTaskList,    "--> ").