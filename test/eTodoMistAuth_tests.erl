-module(eTodoMistAuth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

todo_list_order_test() ->
    {ok, _} = application:ensure_all_started(crypto),
    Database = spawn(fun database_fixture/0),
    true = register(eTodoDB, Database),
    try
        {ok, Server} = eTodoMistAuth:start_link(),
        try
            Token = <<"test-token">>,
            Session = {<<"test-user">>, erlang:system_time(second) + 60,
                       <<"test-csrf">>},
            sys:replace_state(Server, fun(_) -> #{Token => Session} end),
            Expected = [<<"All tasks">>, <<"alpha">>, <<"Beta">>, <<"Zulu">>],
            lists:foreach(
              fun(List) ->
                  {ok, Data} = eTodoMistAuth:todo_data(Token, List, true),
                  Page = json:decode(Data),
                  ?assertEqual(Expected, maps:get(<<"lists">>, Page)),
                  ?assertEqual(List, maps:get(<<"selected_list">>, Page))
              end, [<<"All tasks">>, <<"Zulu">>]),
            ?assertEqual({error, <<"invalid_list">>},
                         eTodoMistAuth:todo_data(Token, <<"Missing">>, true)),
            ?assertEqual({error, invalid_csrf},
                         eTodoMistAuth:move_task(
                           Token, <<"wrong-csrf">>, <<"alpha">>,
                           <<"77">>, <<>>)),
            application:set_env(eTodo, user, "test-user"),
            try
                ?assertEqual({ok, nil},
                             eTodoMistAuth:move_task(
                               Token, <<"test-csrf">>, <<"alpha">>,
                               <<"77">>, <<>>))
            after
                application:unset_env(eTodo, user)
            end
        after
            gen_server:stop(Server)
        end
    after
        Monitor = monitor(process, Database),
        Database ! stop,
        receive
            {'DOWN', Monitor, process, Database, normal} -> ok
        after 1000 ->
            error(database_fixture_did_not_stop)
        end
    end.

database_fixture() ->
    receive
        {'$gen_call', From, {readUserCfg, "test-user"}} ->
            gen_server:reply(From, #userCfg{lists = ["Zulu", "All tasks",
                                                    "alpha", "Beta",
                                                    "All tasks"]}),
            database_fixture();
        {'$gen_call', From, {getETodos, "test-user", "alpha", [],
                             "", undefined}} ->
            gen_server:reply(From, [#etodo{uidDB = 77}]),
            database_fixture();
        {'$gen_call', From, {moveTo, "test-user", _Parent, 77, undefined}} ->
            gen_server:reply(From, {atomic, ok}),
            database_fixture();
        {'$gen_call', From, {getETodos, "test-user", _List, _Filter,
                             _Search, _Cfg}} ->
            gen_server:reply(From, []),
            database_fixture();
        {'$gen_call', From, {getReminders, "test-user"}} ->
            gen_server:reply(From, []),
            database_fixture();
        stop ->
            ok
    end.
