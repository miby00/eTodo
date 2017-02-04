%%%-------------------------------------------------------------------
%%% @author "mikael.bylund@dunderbo.nu"
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. okt 2015 09:40
%%%-------------------------------------------------------------------
-module(eTodoAPI).
-author("mikael.bylund@dunderbo.nu").

%% API
-export([launchBrowser/1]).

launchBrowser(URL) ->
    eTodo:launchBrowser(URL).

