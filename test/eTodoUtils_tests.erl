-module(eTodoUtils_tests).

-include_lib("eunit/include/eunit.hrl").

sort_task_lists_test() ->
    ?assertEqual(["All tasks"], eTodoUtils:sortTaskLists([])),
    ?assertEqual(
       ["All tasks", "Aardvark", "ALL tasks", "Alpha", "alpha", "apple",
        "beta", "zulu", "Älg", "Åska", "Öl"],
       eTodoUtils:sortTaskLists(
         ["zulu", "beta", "Alpha", "apple", "ALL tasks", "All tasks",
          "alpha", "All tasks", "Aardvark", "Åska", "Älg", "Öl"])).
