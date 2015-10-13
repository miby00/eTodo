%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_thunderlink).

-export([getName/0, getDesc/0, getMenu/0]).

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
         eMenuEvent/4]).

getName() -> "Thunderlink".

getDesc() -> "Support for thunderlinks to emails.".

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
getMenu() -> [{70000, "Open email"}].

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
%% @spec eMenuEvent(EScriptDir, User, MenuOption) -> ok
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, _User, _MenuOption, ETodo) ->
    Desc = ETodo#etodo.description,
    REXP = "((([a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+" ++
           "(\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)" ++
           "*)|(\"(([\x01-\x08\x0B\x0C\x0E-\x1F\x7F]" ++
           "|[\x21\x23-\x5B\x5D-\x7E])|(\\[\x01-\x09\x0B" ++
           "\x0C\x0E-\x7F]))*\"))@(([a-zA-Z0-9!#$%&'*+/=?" ++
           "^_`{|}~-]+(\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~" ++
           "-]+)*)|(\[(([\x01-\x08\x0B\x0C\x0E-\x1F\x7F]" ++
           "|[\x21-\x5A\x5E-\x7E])|(\\[\x01-\x09\x0B\x0C\x0E" ++
           "-\x7F]))*\])))",
    LINK = "[tT][hH][uU][nN][dD][eE][rR][lL][iI][nN][kK]://",
    case re:run(Desc, LINK) of
        {match, [{Pos, Len}]} ->
            Text = string:substr(Desc, Pos + Len + 1),
            case re:run(Text, REXP) of
                {match, [{Pos2, Len2}]} ->
                    TLINK = "thunderlink://" ++
                        string:substr(Text, Pos2 + Len2 + 1),
                    io:format("~s~n", [TLINK]);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.
