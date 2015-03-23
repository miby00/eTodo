%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 4 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eRows_lists).
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
         replace/2,
         swapRows/3,
	 toList/1]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
new() ->
    [].

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
toList(Rows) ->
    Rows.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
replace(NewETodos, _OldETodos) ->
    NewETodos.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
empty([]) ->
    true;
empty(_) ->
    false.

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
findIndex(Uid, ETodos) ->
    findIndex(Uid, ETodos, 0).

findIndex(_Uid, [], _) ->
        -1;
findIndex(Uid, [ETodo = #etodo{uidDB = Uid}|_Rest], Index) ->
    {Index, ETodo};
findIndex(Uid, [_ETodo|Rest], Index) ->
    findIndex(Uid, Rest, Index + 1).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
getETodoAtIndex(_,     [])              ->
    undefined;
getETodoAtIndex(0,     [ETodo|_ETodos]) ->
    ETodo;
getETodoAtIndex(Index, [_ETodo|ETodos]) -> getETodoAtIndex(Index - 1, ETodos).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
updateRows(ETodo, Rows) ->
    updateRows(ETodo, Rows, []).

updateRows(ETodo, [], Acc) ->
    eLog:log(debug, ?MODULE, updateRows, [ETodo],
        "Row not found.", ?LINE),
    lists:reverse(Acc);
updateRows(ETodo = #etodo{uidDB = Uid}, [#etodo{uidDB = Uid}|Rest], Acc) ->
    lists:reverse(Acc) ++ [ETodo | Rest];
updateRows(ETodo, [ETodo2|Rest], Acc) ->
    updateRows(ETodo, Rest, [ETodo2|Acc]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
insertRow(ETodo, Row, Rows) ->
    insertRow(ETodo, Rows, Row, []).

insertRow(ETodo, [], _, Acc) ->    %% Add to end
    lists:reverse([ETodo|Acc]);
insertRow(ETodo, Rows, 0, Acc) ->  %% Insert into list
    lists:reverse(Acc) ++ [ETodo|Rows];
insertRow(ETodo, [ETodo2|Rest], Row, Acc) ->
    insertRow(ETodo, Rest, Row - 1, [ETodo2|Acc]).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
deleteRow(Uid, Rows) ->
    lists:keydelete(Uid, #etodo.uidDB, Rows).

%%======================================================================
%% Function :
%% Purpose  :
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
swapRows(#etodo{uidDB = Uid1}, #etodo{uidDB = Uid2}, Rows) ->
    swapRows(Uid1, Uid2, Rows, Rows, []).

swapRows(Uid1, Uid2, [], Rows, Acc) ->
    eLog:log(error, ?MODULE, swapRows, [Uid1, Uid2, Acc],
        "Failed to find rows to swap.", ?LINE),
    Rows;
swapRows(Uid1, Uid2, [ETodo1 = #etodo{uidDB = Uid1},
    ETodo2 = #etodo{uidDB = Uid2}|Rest], _Rows, Acc) ->
    lists:reverse(Acc) ++ [ETodo2, ETodo1|Rest];
swapRows(Uid1, Uid2, [ETodo2 = #etodo{uidDB = Uid2},
    ETodo1 = #etodo{uidDB = Uid1}|Rest], _Rows, Acc) ->
    lists:reverse(Acc) ++ [ETodo1, ETodo2|Rest];
swapRows(Uid1, Uid2, [Uid|Rest], Rows, Acc) ->
    swapRows(Uid1, Uid2, Rest, Rows, [Uid|Acc]).


