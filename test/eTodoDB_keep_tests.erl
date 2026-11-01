-module(eTodoDB_keep_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

keep_task_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> ?_test(check_keep()) end}.

setup() ->
    _ = application:stop(mnesia),
    Dir = filename:join(
            "/tmp",
            "etodo-keep-" ++
                integer_to_list(erlang:unique_integer([positive]))),
    application:set_env(mnesia, dir, Dir),
    application:set_env(eTodo, mode, "noGui"),
    ok = filelib:ensure_dir(filename:join(Dir, "schema.DAT")),
    ok = mnesia:create_schema([node()]),
    {ok, _} = application:ensure_all_started(mnesia),
    {ok, Pid} = eTodoDB:start_link(),
    unlink(Pid),
    Todos = [{1, planning, 0}, {2, done, 1}, {3, planning, 2}],
    {atomic, _} = mnesia:transaction(
                    fun() ->
                            [begin
                                 mnesia:write(#todo{uid = Uid,
                                                    status = Status,
                                                    description = "t"}),
                                 mnesia:write(#userInfo{userName = "u",
                                                        parent = "Work",
                                                        uid = Uid,
                                                        row = Row})
                             end || {Uid, Status, Row} <- Todos]
                    end),
    Dir.

cleanup(Dir) ->
    eTodoDB:stop(),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    file:del_dir_r(Dir),
    application:unset_env(mnesia, dir),
    application:unset_env(eTodo, mode).

check_keep() ->
    HideDone = [?statusDone],
    Uids = fun(ETodos) -> [U || #etodo{uidDB = U} <- ETodos] end,
    ?assertEqual([1, 3],
                 Uids(eTodoDB:getETodos("u", "Work", HideDone, "", undefined))),
    %% The kept task is shown at its place in the list.
    ?assertEqual([1, 2, 3],
                 Uids(eTodoDB:getETodos("u", "Work", HideDone, "", undefined,
                                        2))),
    ?assertEqual([1, 3],
                 Uids(eTodoDB:getETodos("u", "Work", HideDone, "", undefined,
                                        1))),
    %% A task that is not in the list is not added.
    ?assertEqual([1, 3],
                 Uids(eTodoDB:getETodos("u", "Work", HideDone, "", undefined,
                                        99))),
    ?assertEqual(3, eTodoDB:countTodos("u", "Work")),
    ?assertEqual(0, eTodoDB:countTodos("u", "Other")).
