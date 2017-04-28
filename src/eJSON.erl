%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 18 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eJSON).

%% API
-export([makeUsersObj/0,
         makeWebSettingsObj/1,
         makeTodoList/5,
         makeForm/2]).

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("inets/include/httpd.hrl").

-import(eTodoUtils, [toStr/1, toStr/2, makeStr/1, default/2]).

-import(eHtml, [sortETodos/2]).

%%%=====================================================================
%%% API
%%%=====================================================================

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeUsersObj() ->
    Users1 = [{User, string:to_lower(User)} || User <- eTodoDB:getUsers()],
    Users2 = lists:keysort(2, Users1),
    Users3 = [User || {User, _} <- Users2],
    "{\"Users\":" ++ io_lib:format("~p", [Users3]) ++ "}".

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeWebSettingsObj(User) ->
    UserCfg  = eTodoDB:readUserCfg(User),
    Settings = default(UserCfg#userCfg.webSettings, []),
    ["{", makeJSONKVList(Settings)].

makeJSONKVList([]) ->
    "}";
makeJSONKVList([{Key, Value}]) when is_atom(Value) or is_integer(Value) ->
    ["\"", Key, "\":", toStr(Value), "}"];
makeJSONKVList([{Key, Value}|Rest]) when is_atom(Value) or is_integer(Value) ->
    ["\"", Key, "\":", toStr(Value), ",", makeJSONKVList(Rest)];
makeJSONKVList([{Key, Value}]) ->
    ["\"", Key, "\":\"", Value, "\"}"];
makeJSONKVList([{Key, Value}|Rest]) ->
    ["\"", Key, "\":\"", Value, "\",", makeJSONKVList(Rest)].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeTodoList(User, List, Filter, SearchText, Cfg) ->
    ETodos = eTodoDB:getETodos(User, List, Filter, SearchText, Cfg),
    SortedETodos = lists:reverse(sortETodos(User, ETodos)),
    ["[",[makeJSONTodoList(SortedETodos)],"]"].

%%======================================================================
%% Function : makeJSONTodoList
%% Purpose  : Maps makeJsonTodo onto a list of todos but minds the
%%                        ',' between the records
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeJSONTodoList([]) ->
    [];
makeJSONTodoList([ETodo]) ->
    [makeJsonTodo(ETodo)];
makeJSONTodoList([ETodo|ETodos]) ->
    [makeJsonTodo(ETodo),",",makeJSONTodoList(ETodos)].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeJsonTodo(#etodo{uid         = Uid,
                    status      = Status,
                    priority    = Priority,
                    dueTime     = DueTime,
                    description = Description,
                    comment     = Comment,
                    sharedWith  = SharedWith,
                    createTime  = CreateTime,
                    doneTime    = DoneTime,
                    progress    = Progress,
                    owner       = Owner,
                    hasSubTodo  = HasSubTodo,
                    lists = Lists}) ->
    ProgStr = toStr(Progress),
    [
        "{",
        "\"Uid\":\"",           makeJSONString(Uid),
        "\",\"Description\":\"",makeJSONString(Description),
        "\",\"Status\":\"",     Status,
        "\",\"Priority\":\"",   makeJSONString(Priority),
        "\",\"DueTime\":\"",    makeJSONString(DueTime),
        "\",\"Comment\":\"",    makeJSONString(Comment),
        "\",\"SharedWith\":\"", makeJSONString(SharedWith),
        "\",\"CreateTime\":\"", makeJSONString(CreateTime),
        "\",\"DoneTime\":\"",   makeJSONString(DoneTime),
        "\",\"Progress\":\"",   ProgStr,
        "\",\"Owner\":\"",      (Owner),
        "\",\"HasSubTodo\":",   atom_to_list(HasSubTodo),
        ",\"Lists\":\"",   makeJSONString(Lists),
        "\"}"
    ].


%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
makeForm(User, _Default) ->
    TodoLists =
        case eTodoDB:readUserCfg(User) of
            #userCfg{lists = undefined} ->
                [?defTaskList];
            #userCfg{lists = Lists} ->
                Lists
        end,
    ["[",
     makeJSONListsList(TodoLists), "]"].

makeJSONListsList([]) ->
    [];
makeJSONListsList([Value|[]]) ->
    Value2 = eGuiFunctions:filterAndConvert2Utf8(Value),
    ["{\"list\":\"", Value2, "\"}"];
makeJSONListsList([Value|Rest]) ->
    Value2 = eGuiFunctions:filterAndConvert2Utf8(Value),
    ["{\"list\":\"", Value2, "\"},",makeJSONListsList(Rest)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

makeJSONString(Text) ->
    eGuiFunctions:filterAndConvert2Utf8(quote(Text, [])).

quote("", SoFar)  -> lists:flatten(SoFar);

%% Row
quote([13, 10|Rest], SoFar) -> quote(Rest, [SoFar, "<br />"]);
quote([10|Rest],     SoFar) -> quote(Rest, [SoFar, "<br />"]);
quote([Char|Rest],   SoFar) when (Char =< 16#F) ->
    quote(Rest, [SoFar, "\\u000" ++ integer_to_list(Char, 16)]);
quote([Char|Rest],   SoFar) when (Char =< 16#19) or
                                 (Char == 16#22) or
                                 (Char == 16#5C) ->
    quote(Rest, [SoFar, "\\u00" ++ integer_to_list(Char, 16)]);
quote([Char|Rest], SoFar)   -> quote(Rest, [SoFar, Char]).

