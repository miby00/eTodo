-module(eTodoNotification_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eTodo/include/eTodo.hrl").

notification_links_test() ->
    with_fixture(
      fun(_Database) ->
              ?assertEqual(
                 [<<"https://relay.example:9005/messages?kind=chat">>],
                 links(eMime:notificationLinks("test", [message]))),
              ?assertEqual(
                 [<<"https://relay.example:9005/messages?kind=reminders">>],
                 links(eMime:notificationLinks("test", [reminder]))),
              ?assertEqual(
                 [<<"https://relay.example:9005/messages?kind=chat">>,
                  <<"https://relay.example:9005/messages?kind=reminders">>],
                 links(eMime:notificationLinks(
                         "test", [reminder, message, reminder]))),
              application:set_env(eTodo, mistWebLinkHost, "relay.example:9005/"),
              ?assertEqual("https://relay.example:9005",
                           eTodoMist:web_base_url("test")),
              application:unset_env(eTodo, mistWebLinkHost),
              ?assertEqual("https://desktop.example:9003",
                           eTodoMist:web_base_url("test")),
              ?assertEqual("https://localhost:9003",
                           eTodoMist:web_base_url("missing")),
              application:set_env(eTodo, mistWebEnabled, false),
              ?assertEqual(<<>>, eMime:notificationLinks("test", [message])),
              ?assertEqual([], links(eHtml:generateAlarmMsgEmail(
                                      "test", 42, "Reminder")))
      end).

headless_notification_links_test() ->
    with_fixture(
      fun(Database) ->
              lists:foreach(
                fun({Types, Expected}) ->
                        Records = [{os:timestamp(), Type, "Ann", [],
                                    <<"<p>Unread &amp; useful</p>">>}
                                   || Type <- Types],
                        Database ! {messages, Records},
                        put(notifySince, {0, 0, 0}),
                        {noreply, _} = eTodoNoGui:handle_info(
                                         sendNotification,
                                         #guiState{user = "test", reply = "Ann"}),
                        Mail = received_mail(),
                        ?assertEqual(Expected, links(Mail)),
                        ?assertNotEqual(nomatch,
                                        binary:match(Mail,
                                                     <<"Unread &amp; useful">>))
                end,
                [{[msgEntry], [<<"https://relay.example:9005/messages?kind=chat">>]},
                 {[alarmEntry],
                  [<<"https://relay.example:9005/messages?kind=reminders">>]},
                 {[msgEntry, alarmEntry, alarmEntry],
                  [<<"https://relay.example:9005/messages?kind=chat">>,
                   <<"https://relay.example:9005/messages?kind=reminders">>]}])
      end).

desktop_notification_links_test() ->
    with_fixture(
      fun(_Database) ->
              {noreply, _} = eTodo:handle_info(
                               sendNotification,
                               #guiState{user = "test", reply = "Ann"}),
              ?assertEqual(
                 [<<"https://relay.example:9005/messages?kind=chat">>],
                 links(received_mail())),
              {ok, Events} = gen_event:start_link({local, ePeerEM}),
              try
                  {noreply, _, 30000} = eTodoAlarm:handle_info(
                                         timeout, {state, "test"}),
                  Reminder = received_mail(),
                  ?assert(lists:member(
                            <<"https://relay.example:9005/messages?kind=reminders">>,
                            links(Reminder))),
                  ?assert(lists:member(
                            <<"https://relay.example:9005/?list=All%20tasks&all=1#task-42">>,
                            links(Reminder)))
              after
                  gen_event:stop(Events)
              end
      end).

with_fixture(Test) ->
    Settings = [{mistWebEnabled, true},
                {mistWebLinkHost, "relay.example:9005"},
                {mistWebPort, 9003},
                {notificationEmail, "owner@example.test"},
                {mode, "gui"}],
    Old = [{Key, application:get_env(eTodo, Key)} || {Key, _} <- Settings],
    [application:set_env(eTodo, Key, Value) || {Key, Value} <- Settings],
    Database = spawn_link(fun() -> database([]) end),
    true = register(eTodoDB, Database),
    true = register(eSMTP, self()),
    try
        Test(Database)
    after
        unregister(eSMTP),
        Ref = monitor(process, Database),
        Database ! stop,
        receive {'DOWN', Ref, process, Database, normal} -> ok end,
        [case Value of
             {ok, Previous} -> application:set_env(eTodo, Key, Previous);
             undefined -> application:unset_env(eTodo, Key)
         end || {Key, Value} <- Old]
    end.

database(Records) ->
    receive
        {messages, NewRecords} ->
            database(NewRecords);
        {'$gen_call', From, {getMessageList, "test", _, _}} ->
            gen_server:reply(From, Records),
            database(Records);
        {'$gen_call', From, {getConnection, "missing"}} ->
            gen_server:reply(From, undefined),
            database(Records);
        {'$gen_call', From, {getConnection, _}} ->
            gen_server:reply(From, #conCfg{host = "desktop.example",
                                         email = "owner@example.test"}),
            database(Records);
        {'$gen_call', From, {getReminders, "test"}} ->
            gen_server:reply(From, [#alarmCfg{userName = "test", uid = 42,
                                            startDate = {2020, 1, 1},
                                            nextAlarm = {{2020, 1, 1}, {0, 0, 0}},
                                            recurrence = none,
                                            emailReminder = true}]),
            database(Records);
        {'$gen_call', From, {getTodo, 42}} ->
            gen_server:reply(From, #todo{uid = 42, status = planning,
                                        description = "Reminder"}),
            database(Records);
        {'$gen_call', From, {addReminder, _}} ->
            gen_server:reply(From, {atomic, ok}),
            database(Records);
        stop ->
            ok
    end.

received_mail() ->
    receive
        {'$gen_cast', {sendMail, _, _, Mail}} ->
            Bin = iolist_to_binary(Mail),
            [_, Rest] = binary:split(
                          Bin, <<"Content-Transfer-Encoding: base64\r\n\r\n">>),
            [Body | _] = binary:split(Rest, <<"\r\n------=_Part_">>),
            base64:decode(binary:replace(Body, <<"\r\n">>, <<>>, [global]))
    after 1000 ->
        error(notification_email_not_created)
    end.

links(Html) ->
    link_nodes(mochiweb_html:parse(
                 iolist_to_binary(["<div>", Html, "</div>"]))).

link_nodes({<<"a">>, Attributes, Children}) ->
    [proplists:get_value(<<"href">>, Attributes) |
     lists:append([link_nodes(Child) || Child <- Children])];
link_nodes({_, _, Children}) ->
    lists:append([link_nodes(Child) || Child <- Children]);
link_nodes(_) ->
    [].
