%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 4 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eRows).
-author("mikael.bylund@gmail.com").

-include("eTodo.hrl").

%% API
-export([deleteRow/2,
         empty/1,
         findIndex/2,
         getETodoAtIndex/2,
         insertRow/3,
         updateRows/2,
         new/0,
         toList/1,
         replace/2,
         swapRows/3]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
new() ->
    ets:new(eRowsTab, [set, named_table, protected, {keypos, 1}]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
toList(TRef) ->
    toList(TRef, 0, []).

toList(TRef, Index, SoFar) ->
    case getETodoAtIndex(Index, TRef) of
        undefined ->
            lists:reverse(SoFar);
        ETodo ->
            toList(TRef, Index + 1, [ETodo | SoFar])
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
replace(NewETodos, TRef) ->
    ets:delete_all_objects(TRef),
    replace(NewETodos, 0, TRef).

replace([], _Index, TRef) ->
    TRef;
replace([ETodo|Rest], Index, TRef) ->
    ets:insert(TRef, {Index, ETodo}),
    ets:insert(TRef, {{uid, ETodo#etodo.uidDB}, Index}),
    replace(Rest, Index + 1, TRef).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
empty(TRef) ->
    ets:info(TRef, size) == 0.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
findIndex(Uid, TRef) ->
    case ets:lookup(TRef, {uid, Uid}) of
        [{{uid, Uid}, Index}] ->
            case getETodoAtIndex(Index, TRef) of
                undefined ->
                   -1;
                ETodo ->
                    {Index, ETodo}
            end;
        _ ->
            -1
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getETodoAtIndex(Index, TRef) ->
    case ets:lookup(TRef, Index) of
        [{Index, ETodo}] ->
            ETodo;
        _ ->
            undefined
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateRows(ETodo, TRef) ->
    case findIndex(ETodo#etodo.uidDB, TRef) of
        -1 ->
            eLog:log(debug, ?MODULE, updateRows, [ETodo],
                     "Row not found.", ?LINE);
        {Index, OldETodo} ->
            ets:delete(TRef, {uid, OldETodo#etodo.uidDB}),
            ets:insert(TRef, {Index, ETodo}),
            ets:insert(TRef, {{uid, ETodo#etodo.uidDB}, Index})
    end,
    TRef.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
insertRow(ETodo, Row, TRef) ->
    case getETodoAtIndex(Row, TRef) of
        undefined ->
            ets:insert(TRef, {Row, ETodo}),
            ets:insert(TRef, {{uid, ETodo#etodo.uidDB}, Row}),
            TRef;
        ETodoToMove ->
            ets:delete(TRef, {uid, ETodoToMove#etodo.uidDB}),
            ets:insert(TRef, {Row, ETodo}),
            ets:insert(TRef, {{uid, ETodo#etodo.uidDB}, Row}),
            insertRow(ETodoToMove, Row + 1, TRef)
    end.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
deleteRow(Uid, TRef) ->
    L1 = toList(TRef),
    L2 = lists:keydelete(Uid, #etodo.uidDB, L1),
    replace(L2, TRef).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
swapRows(ETodo1, ETodo2, TRef) ->
    case catch swapRows2(ETodo1, ETodo2, TRef) of
        {'EXIT', _ } ->
            eLog:log(error, ?MODULE, swapRows, [ETodo1, ETodo2],
                "Failed to find rows to swap.", ?LINE),
            TRef;
        Result ->
            Result
    end.

swapRows2(ETodo1 = #etodo{uidDB = Uid1}, ETodo2 = #etodo{uidDB = Uid2}, TRef) ->
    {Index1, _} = findIndex(Uid1, TRef),
    {Index2, _} = findIndex(Uid2, TRef),
    ets:insert(TRef, {{uid, Uid1}, Index2}),
    ets:insert(TRef, {{uid, Uid2}, Index1}),
    ets:insert(TRef, {Index2, ETodo1}),
    ets:insert(TRef, {Index1, ETodo2}),
    TRef.
