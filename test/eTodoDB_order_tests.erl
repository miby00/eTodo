-module(eTodoDB_order_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

reorder_todos_test_() ->
    {setup, fun setup_mnesia/0, fun cleanup_mnesia/1,
     fun reorder_todos/1}.

setup_mnesia() ->
    _ = application:stop(mnesia),
    Dir = filename:join(
            "/tmp",
            "etodo-order-" ++
                integer_to_list(erlang:unique_integer([positive]))),
    application:set_env(mnesia, dir, Dir),
    ok = filelib:ensure_dir(filename:join(Dir, "schema.DAT")),
    ok = mnesia:create_schema([node()]),
    {ok, _} = application:ensure_all_started(mnesia),
    {atomic, ok} = mnesia:create_table(
                     userInfo,
                     [{attributes, record_info(fields, userInfo)},
                      {type, bag}]),
    {atomic, _} = mnesia:transaction(
                    fun() ->
                            [mnesia:write(#userInfo{userName = "test",
                                                    parent = 10,
                                                    uid = Uid,
                                                    row = Row}) ||
                                {Uid, Row} <- [{1, 0}, {2, 1}, {3, 2}]]
                    end),
    Dir.

cleanup_mnesia(Dir) ->
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    file:del_dir_r(Dir),
    application:unset_env(mnesia, dir).

reorder_todos(_Dir) ->
    ?_test(check_reordering()).

check_reordering() ->
    ok = code:add_paths(filelib:wildcard("web/backend/build/dev/erlang/*/ebin")),
    {ok, Push} = eTodoPush:start_link(),
    try
        Subject = 'gleam@erlang@process':new_subject(),
        nil = eTodoPush:subscribe("test", Subject),
        State = {state, [], [], normal},
        {reply, {atomic, ok}, _DownState} =
            eTodoDB:handle_call({moveDown, "test", 10, 1, 2},
                                self(), State),
        assert_order_push(Subject),
        ?assertEqual([2, 1, 3], todo_order()),
        {reply, {atomic, ok}, _UpState} =
            eTodoDB:handle_call({moveUp, "test", 10, 2, 1},
                                self(), State),
        assert_order_push(Subject),
        ?assertEqual([1, 2, 3], todo_order()),
        {reply, {atomic, ok}, MovedState} =
            eTodoDB:handle_call({moveTo, "test", 10, 3, 1},
                                self(), State),
        assert_order_push(Subject),
        ?assertEqual([3, 1, 2], todo_order()),
        UndoState = setelement(4, MovedState, undo),
        {reply, {atomic, ok}, RestoredState} =
            eTodoDB:handle_call(
              {restoreTodoOrder, "test", 10, [1, 2, 3]},
              self(), UndoState),
        assert_order_push(Subject),
        ?assertEqual(normal, element(4, RestoredState)),
        ?assertEqual([1, 2, 3], todo_order()),
        {reply, {atomic, ok}, _BottomState} =
            eTodoDB:handle_call({moveTo, "test", 10, 1, undefined},
                                self(), State),
        assert_order_push(Subject),
        ?assertEqual([2, 3, 1], todo_order()),
        ?assertEqual(
           {atomic, {error, not_found}},
           element(2, eTodoDB:handle_call(
                        {moveTo, "test", 10, 1, 999},
                        self(), State))),
        {reply, {atomic, ok}, State} =
            eTodoDB:handle_call({moveTo, "test", 10, 1, undefined},
                                self(), State),
        ?assertEqual({error, nil},
                     'gleam@erlang@process':'receive'(Subject, 100))
    after
        unlink(Push),
        gen_server:stop(Push)
    end.

assert_order_push(Subject) ->
    ?assertEqual({ok, <<"todo_order">>},
                 'gleam@erlang@process':'receive'(Subject, 1000)).

todo_order() ->
    {atomic, Infos} = mnesia:transaction(
                        fun() ->
                                mnesia:match_object(
                                  #userInfo{userName = "test", parent = 10,
                                            _ = '_'})
                        end),
    [Uid || #userInfo{uid = Uid} <-
                lists:keysort(#userInfo.row, Infos)].
