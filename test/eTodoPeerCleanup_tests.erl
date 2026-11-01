-module(eTodoPeerCleanup_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

cleanup_stale_peer_connections_test_() ->
    {setup, fun setup_mnesia/0, fun cleanup_mnesia/1,
     fun cleanup_stale_peer_connections/1}.

setup_mnesia() ->
    _ = application:stop(mnesia),
    Dir = filename:join(
            "/tmp",
            "etodo-peer-cleanup-" ++
                integer_to_list(erlang:unique_integer([positive]))),
    application:set_env(mnesia, dir, Dir),
    ok = filelib:ensure_dir(filename:join(Dir, "schema.DAT")),
    ok = mnesia:create_schema([node()]),
    {ok, _} = application:ensure_all_started(mnesia),
    {atomic, ok} = mnesia:create_table(
                     conCfg, [{attributes, record_info(fields, conCfg)}]),
    {atomic, ok} = mnesia:create_table(
                     peerSeen, [{attributes, [userName, lastSeen]}]),
    Dir.

cleanup_mnesia(Dir) ->
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    file:del_dir_r(Dir),
    application:unset_env(mnesia, dir).

cleanup_stale_peer_connections(_Dir) ->
    ?_test(check_stale_peer_cleanup()).

check_stale_peer_cleanup() ->
    Now = calendar:datetime_to_gregorian_seconds(eTodoUtils:dateTime()),
    Cutoff = calendar:gregorian_seconds_to_datetime(Now - 60 * 24 * 60 * 60),
    Stale = calendar:gregorian_seconds_to_datetime(Now - 61 * 24 * 60 * 60),
    Recent = calendar:gregorian_seconds_to_datetime(Now - 59 * 24 * 60 * 60),
    Connections =
        [#conCfg{userName = "stale", updateTime = Recent},
         #conCfg{userName = "recent", updateTime = Recent},
         #conCfg{userName = "configured", updateTime = configured},
         #conCfg{userName = "protected", updateTime = Recent}],
    Seen = [{peerSeen, "stale", Stale},
            {peerSeen, "recent", Recent},
            {peerSeen, "configured", Stale},
            {peerSeen, "protected", Stale}],
    {atomic, _} = mnesia:transaction(
                    fun() ->
                            [mnesia:write(Connection) ||
                                Connection <- Connections],
                            [mnesia:write(Entry) || Entry <- Seen]
                    end),
    State = {state, [], [], normal},
    {reply, {atomic, ["stale"]}, State} =
        eTodoDB:handle_call({removeStaleConnections, Cutoff, ["protected"]},
                            self(), State),
    ?assertEqual([], mnesia:dirty_read(conCfg, "stale")),
    ?assertEqual([], mnesia:dirty_read(peerSeen, "stale")),
    ?assertMatch([#conCfg{}], mnesia:dirty_read(conCfg, "recent")),
    ?assertMatch([#conCfg{updateTime = configured}],
                 mnesia:dirty_read(conCfg, "configured")),
    ?assertMatch([#conCfg{}], mnesia:dirty_read(conCfg, "protected")).
