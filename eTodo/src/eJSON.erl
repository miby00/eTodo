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
-export([makeTodoList/5,
         makeForm/2]).

-include("eTodo.hrl").
-include_lib("inets/include/httpd.hrl").

-import(eTodoUtils, [toStr/1, toStr/2, makeStr/1]).

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
makeTodoList(User, List, Filter, SearchText, Cfg) ->
    ETodos = eTodoDB:getETodos(User, List, Filter, SearchText, Cfg),
    %%["[",[makeHtmlTodoJSON(ETodo) || ETodo <- ETodos],"]"].
    ["[",[makeJSONTodoList(ETodos)],"]"].

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
    Value2 = unicode:characters_to_binary(Value, utf8),
    ["{\"list\":\"", Value2, "\"}"];
makeJSONListsList([Value|Rest]) ->
    Value2 = unicode:characters_to_binary(Value, utf8),
    ["{\"list\":\"", Value2, "\"},",makeJSONListsList(Rest)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

makeJSONString(Text) ->
    unicode:characters_to_binary(quote(Text, []), utf8).

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

