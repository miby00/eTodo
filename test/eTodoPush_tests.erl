-module(eTodoPush_tests).

-include_lib("eunit/include/eunit.hrl").

push_test() ->
    ok = code:add_paths(filelib:wildcard("web/backend/build/dev/erlang/*/ebin")),
    {ok, Server} = eTodoPush:start_link(),
    try
        Subject = 'gleam@erlang@process':new_subject(),
        nil = eTodoPush:subscribe(<<"me">>, Subject),
        eTodoPush:messages_changed("other", msgEntry),
        eTodoPush:messages_changed("me", alarmEntry),
        ?assertEqual({ok, <<"reminders">>},
                     'gleam@erlang@process':'receive'(Subject, 1000)),
        ?assertEqual({error, nil},
                     'gleam@erlang@process':'receive'(Subject, 100))
    after
        unlink(Server),
        gen_server:stop(Server)
    end.
