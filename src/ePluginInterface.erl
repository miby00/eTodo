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
-export([toStr/1,
         toClipboard/1,
         getLoggedWork/3,
         getAllLoggedWorkDate/1,
         getTime/1,
         makeRef/0,
         dateTime/0,
         getTaskList/0,
         getRow/2,
         addTodo/2,
         tryInt/1,
         todoCreated/3,
         saveTime/3,
         readConfig/1,
         saveConfig/2]).

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
%% @spec readConfig(Module) -> [terms]
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