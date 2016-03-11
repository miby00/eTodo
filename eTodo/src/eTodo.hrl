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
                   sortColsDlg, timerDlg, timerRef, pluginDlg, logWorkDlg}).

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
                   plugins         = []}).

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

-define(assigned,         1065).

-define(about,            1070).

-define(showcolumns,      1075).

-define(sortColumns,      1099).

-define(lists,            1100).

-define(bookmarks,        1200).

-define(plugins,          1300).


%% Constants for gui
-define(clearMsgText,  "Clear \"All Messages\" tab").
-define(clearRemText,  "Clear \"Reminders\" tab").

-define(descSortDef,    "Sort order default").
-define(descSortAsc,    "Sort order ascending").
-define(descSortDec,    "Sort order descending").

-define(descNone,       "").
-define(descNA,         "N/A").

-define(descPlanning,   "Planning").
-define(descInProgress, "In progress").
-define(descDone,       "Done").

-define(descLow,        "Low").
-define(descMedium,     "Medium").
-define(descHigh,       "High").

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

-define(sbNotLoggedIn,  "Not logged in").

-define(defTaskList,    "All tasks").

-define(defInbox,       "Inbox").
-define(defLoggedWork,  "-- Logged work --").
-define(defShowStatus,  "-- Show status --").

-define(subTaskList,    "--> ").
