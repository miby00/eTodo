%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2016, Mikael Bylund
%%% @doc
%%% Collection of functions used by plugins.
%%% @end
%%% Created : 09 February 2016 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(ePluginInterface).
-author("mikael.bylund@gmail.com").

%% API
-export([addTodo/2,
         appendToPage/1,
         dateTime/0,
         getAllLoggedWorkDate/1,
         getExternalUsers/1,
         getPeerInfo/1,
         getLoggedWork/3,
         getRow/2,
         getTaskList/0,
         getTime/1,
         getWeekDay/1,
         getWorkLog/2,
         incDate/2,
         loggedIn/0,
         makeRef/0,
         msgEntry/3,
         readConfig/1,
         saveConfig/2,
         saveExternalUser/3,
         saveTime/3,
         setPortrait/3,
         systemEntry/1,
         toClipboard/1,
         todoCreated/3,
         toStr/1,
         tryInt/1]).

-include_lib("eTodo/include/eTodo.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Convert date to string
%% @spec toStr(Date) -> string()
%%
%% @end
%%--------------------------------------------------------------------
toStr(Date) ->
    eTodoUtils:toStr(Date).

%%--------------------------------------------------------------------
%% @doc
%% Get External Users
%% @spec getExternalUsers(User) -> ExternalUsers
%%
%% @end
%%--------------------------------------------------------------------
getExternalUsers(User) ->
    UserCfg = eTodoDB:readUserCfg(User),
    UserCfg#userCfg.ownerCfg.

%%--------------------------------------------------------------------
%% @doc
%% Get External Users
%% @spec saveExternalUser(User         :: string(),
%%                        ExternalUser :: binary(),
%%                        Address      :: binary()) -> ok
%%
%% @end
%%--------------------------------------------------------------------
saveExternalUser(User, ExternalUser, Address) ->
    UserCfg   = eTodoDB:readUserCfg(User),
    ExtUser   = <<ExternalUser/binary, "<", Address/binary, ">">>,
    ExtUser2  = binary_to_list(ExtUser),
    ExtUsers  = UserCfg#userCfg.ownerCfg,
    ExtUsers2 = [ExtUser2|lists:delete(ExtUser2, ExtUsers)],
    case ExtUsers of
        ExtUsers2 ->
            %% No change, no need to update chat window.
            ok;
        _ ->
            eTodoDB:saveUserCfg(UserCfg#userCfg{ownerCfg = ExtUsers2}),
            eTodo:updateExternalUsers()
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get peer information
%% @spec getPeerInfo(ExternalUser) -> {User, Address}
%%
%% @end
%%--------------------------------------------------------------------
getPeerInfo(ExtUser) ->
    eTodoUtils:getPeerInfo(ExtUser).

%%--------------------------------------------------------------------
%% @doc
%% Put string in clipboard
%% @spec toClipboard(String) -> void()
%%
%% @end
%%--------------------------------------------------------------------
toClipboard(String) ->
    eGuiFunctions:toClipboard(String).

%%--------------------------------------------------------------------
%% @doc
%% Get logged work from database
%% @spec getLoggedWork(User, Uid, Date) -> {date(), Hours, Min}
%%
%% @end
%%--------------------------------------------------------------------
getLoggedWork(User, Uid, Date) ->
    eTodoDB:getLoggedWork(User, Uid, Date).

%%--------------------------------------------------------------------
%% @doc
%% Get all logged time for date
%% @spec getAllLoggedWorkDate(Uid) -> [string()]
%%
%% @end
%%--------------------------------------------------------------------
getAllLoggedWorkDate(Uid) ->
    eTodoDB:getAllLoggedWorkDate(Uid).

%%--------------------------------------------------------------------
%% @doc
%% Get estimate and remaining time from database.
%% @spec getTime(Uid) -> {Estimate, Remaining}
%%
%% @end
%%--------------------------------------------------------------------
getTime(Uid) ->
    eTodoDB:getTime(Uid).

%%--------------------------------------------------------------------
%% @doc
%% Make eTodo reference
%% @spec makeRef() -> integer()
%%
%% @end
%%--------------------------------------------------------------------
makeRef() ->
    eTodoUtils:makeRef().

%%--------------------------------------------------------------------
%% @doc
%% Generate date time stamp
%% @spec dateTime() -> {date(), time()}
%%
%% @end
%%--------------------------------------------------------------------
dateTime() ->
    eTodoUtils:dateTime().

%%--------------------------------------------------------------------
%% @doc
%% Get active task list
%% @spec getTaskList() -> string()
%%
%% @end
%%--------------------------------------------------------------------
getTaskList() ->
    eTodo:getTaskList().

%%--------------------------------------------------------------------
%% @doc
%% Get row to add task
%% @spec getRow(User, TaskList) -> integer()
%%
%% @end
%%--------------------------------------------------------------------
getRow(User, TaskList) ->
    eTodoDB:getRow(User, TaskList).

%%--------------------------------------------------------------------
%% @doc
%% add task
%% @spec addTodo(UserInfo, Task) -> ok
%%
%% @end
%%--------------------------------------------------------------------
addTodo(UserInfo, Todo) ->
    eTodoDB:addTodo(UserInfo, Todo).

%%--------------------------------------------------------------------
%% @doc
%% Try conversion to integer
%% @spec tryInt(TaskList) -> integer() | string()
%%
%% @end
%%--------------------------------------------------------------------
tryInt(TaskList) ->
    eTodoUtils:tryInt(TaskList).

%%--------------------------------------------------------------------
%% @doc
%% Update GUI with task
%% @spec todoCreated(TaskList, Row, Task) -> void()
%%
%% @end
%%--------------------------------------------------------------------
todoCreated(TaskList, Row, Todo) ->
    eTodo:todoCreated(TaskList, Row, Todo).

%%--------------------------------------------------------------------
%% @doc
%% Save Estimate and Remaining to database
%% @spec saveTime(Uid, Estimate, Remaining) -> ok
%%
%% @end
%%--------------------------------------------------------------------
saveTime(Uid, Estimate, Remaining) ->
    eTodoDB:saveTime(integer_to_list(Uid),
                     Estimate div 3600, Remaining div 3600).

%%--------------------------------------------------------------------
%% @doc
%% Read config file
%% @spec readConfig(Module) -> Map :: map()
%%
%% @end
%%--------------------------------------------------------------------
readConfig(Module) ->
    {ok, Dir}  = application:get_env(mnesia, dir),
    ConfigFile = filename:join(Dir, atom_to_list(Module) ++ ".dat"),
    case file:consult(ConfigFile) of
        {ok, [Cfg]} when is_map(Cfg) ->
            Cfg;
        _ ->
            #{}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Save config file
%% @spec saveConfig(Module, Config) -> ok
%%
%% @end
%%--------------------------------------------------------------------
saveConfig(Module, Config) ->
    {ok, Dir}  = application:get_env(mnesia, dir),
    ConfigFile = filename:join(Dir, atom_to_list(Module) ++ ".dat"),
    file:write_file(ConfigFile, io_lib:format("~tp.~n", [Config])).

%%--------------------------------------------------------------------
%% @doc
%% Get complete work log for a date
%% @spec getWorkLog(User, Date) -> [{Uid, WorkLogDesc}]
%%
%% @end
%%--------------------------------------------------------------------
getWorkLog(User, Date) ->
    D1 = eTodoDB:getLoggedWork(User, Date),
    D2 = eTodoDB:getLoggedWork(User, incDate(Date, 1)),
    D3 = eTodoDB:getLoggedWork(User, incDate(Date, 2)),
    D4 = eTodoDB:getLoggedWork(User, incDate(Date, 3)),
    D5 = eTodoDB:getLoggedWork(User, incDate(Date, 4)),
    All = lists:flatten([D1, D2, D3, D4, D5]),
    Act1 = [Task || {Task, _H, _M} <- All],
    Act2 = lists:usort(Act1),
    Act3 = [{Task, eTodoDB:getWorkDesc(Task)} || Task <- Act2],
    lists:keysort(2, Act3).

%%--------------------------------------------------------------------
%% @doc
%% Increase date with Inc days.
%% @spec incDate(Date, Inc) -> date()
%%
%% @end
%%--------------------------------------------------------------------
incDate(Date, Inc) ->
    Days = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(Days + Inc).


%%--------------------------------------------------------------------
%% @doc
%% Generate system message
%% @spec systemEntry(Text) -> void()
%%
%% @end
%%--------------------------------------------------------------------
systemEntry(Text) when is_binary(Text) ->
    eTodo:systemEntry(system, unicode:characters_to_list(Text, utf8));
systemEntry(Text) ->
    eTodo:systemEntry(system, Text).

%%--------------------------------------------------------------------
%% @doc
%% Generate msgEntry
%% @spec msgEntry(User, Users, Text) -> void()
%%
%% @end
%%--------------------------------------------------------------------
msgEntry(User, Users, Text) when is_binary(User), is_binary(Text) ->
    Users2 = [unicode:characters_to_list(To, utf8) || To <- Users],
    eTodo:msgEntry(unicode:characters_to_list(User, utf8),
                   Users2, unicode:characters_to_list(Text, utf8));
msgEntry(User, Users, Text) ->
    eTodo:msgEntry(User, Users, Text).

%%--------------------------------------------------------------------
%% @doc
%% Generate own created html message
%% @spec appendToPage(Html) -> void()
%%
%% @end
%%--------------------------------------------------------------------
appendToPage(Html) ->
    eTodo:appendToPage(Html).

%%--------------------------------------------------------------------
%% @doc
%% Get week day
%% @spec getWeekDay(Date) -> weekday
%%
%% @end
%%--------------------------------------------------------------------
getWeekDay(Date) ->
    DayNum = calendar:day_of_the_week(Date),
    lists:nth(DayNum, ["Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday", "Sunday"]).

%%--------------------------------------------------------------------
%% @doc
%% Get user logged in to local eTodo.
%% @spec loggedIn() -> false | {true, User}
%%
%% @end
%%--------------------------------------------------------------------
loggedIn() ->
    eTodo:loggedIn().

%%--------------------------------------------------------------------
%% @doc
%% Set portrait
%% @spec setPortrait(User :: string(), Overwrite :: true | false) -> void
%%
%% @end
%%--------------------------------------------------------------------
setPortrait(User, Picture, Overwrite) ->
    RootDir        = eTodoUtils:getRootDir(),
    CustomPortrait =  RootDir ++ "/Icons/portrait_" ++ User ++ ".png",
    PortraitExist  = filelib:is_file(CustomPortrait),
    savePortrait(CustomPortrait, Picture, PortraitExist, Overwrite).

savePortrait(_, _, true, false)   -> ok;
savePortrait(File, Picture, _, _) -> file:write_file(File, Picture).
