%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_httplink).

-export([getName/0, getDesc/0, getMenu/1]).

-export([eGetStatusUpdate/4,
         eTimerStarted/6,
         eTimerStopped/2,
         eTimerEnded/3,
         eReceivedMsg/4,
         eReceivedSysMsg/2,
         eReceivedAlarmMsg/2,
         eLoggedInMsg/2,
         eLoggedOutMsg/2,
         eSetStatusUpdate/4,
         eMenuEvent/5]).

getName() -> "HTTP(s) Link".

getDesc() -> "Support for finding and opening http/https links.".

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

%%--------------------------------------------------------------------
%% @doc
%% Return key value list of right menu options.
%% Menu option should be a unique integer bigger than 1300.
%% @spec getMenu() -> [{menuOption, menuText}, ...]
%% @end
%%--------------------------------------------------------------------
getMenu(ETodo) ->
    Text = ETodo#etodo.description ++ " " ++ ETodo#etodo.comment,
    REXP = "[[:^space:]]*",
    LINK = "[hH][tT][tT][pP][sS]?://",
    HLinks = getHttpLinks(Text, LINK, REXP, []),
    getMenu(HLinks, 50000, []).

getMenu([], _MenuOption, SoFar) -> SoFar;
getMenu([HLink|Rest], MenuOption, SoFar) ->
    getMenu(Rest, MenuOption + 1, [{MenuOption, HLink}|SoFar]).

%% Calls are only made to plugin.beam

%%--------------------------------------------------------------------
%% @doc
%% Called every 15 seconds to check if someone changes status
%% outside eTodo.
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg) ->
%%       {ok, Status, StatusMsg}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(_Dir, _User, Status, StatusMsg) ->
    %% This function is called every 15 seconds, and should return
    {ok, Status, StatusMsg}.

%% Casts are made to all plugin*.beam

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(EScriptDir, User, Text, Hours, Min, Sec) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStarted(_EScriptDir, _User, _Text, _Hours, _Min, _Sec) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(EScriptDir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerStopped(_EScriptDir, _User) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eTimerEnded(_EScriptDir, _User, _Text) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(_EScriptDir, _User, _Users, _Text) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(_Dir, _Text) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text) -> ok
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(_Dir, _Text) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs in.
%%
%% @spec eLoggedInMsg(Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(_Dir, _User) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs out.
%%
%% @spec eLoggedOutMsg(Dir, User) -> ok
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(_Dir, _User) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg) -> ok
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(_Dir, _User, _Status, _StatusMsg) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Called for right click menu
%%
%% @spec eMenuEvent(EScriptDir, User, MenuOption, ETodo, MenuText) -> ok
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, _User, _MenuOption, _ETodo, MenuText) ->
    eTodoAPI:launchBrowser(MenuText).


getHttpLinks(Text, Link, REXP, SoFar) ->
    case re:run(Text, Link) of
        {match, [{Pos, Len}]} ->
            Text2 = string:substr(Text, Pos + 1 + Len),
            case re:run(Text2, REXP) of
                {match, [{Pos2, Len2}]} ->
                    HLINK = string:substr(Text, Pos + 1, Len) ++
                        string:sub_string(Text2, Pos2 + 1, Len2),
                    Text3 = string:substr(Text2, Pos2 + 1 + Len2),
                    getHttpLinks(Text3, Link, REXP, [HLINK|SoFar]);
                _ ->
                    SoFar
            end;
        _ ->
            SoFar
    end.
