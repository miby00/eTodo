-module(plugin_slack_tests).

-include_lib("eunit/include/eunit.hrl").

%% Plugins are compiled when eTodo starts, compile it the same way.
plugin_test_() ->
    {setup, fun load/0, fun(_) -> ok end,
     [fun slack_to_md/0,
      fun slack_refs/0,
      fun slack_code/0,
      fun slack_emoji/0,
      fun md_to_slack/0,
      fun md_to_slack_code/0,
      fun md_to_slack_bad_utf8/0,
      fun status/0,
      fun dnd/0,
      fun file_name/0]}.

load() ->
    File = filename:join([code:lib_dir(eTodo), "priv", "ePlugins",
                          "plugin_slack.erl"]),
    {ok, Mod, Bin} = compile:file(File, [binary, export_all,
                                         nowarn_export_all, return_errors]),
    {module, Mod} = code:load_binary(Mod, File, Bin).

names({user, <<"U1">>})    -> <<"Anna">>;
names({channel, <<"C1">>}) -> <<"general">>;
names(_)                   -> undefined.

toMD(Text) -> plugin_slack:slackToMD(Text, fun names/1).

slack_to_md() ->
    ?assertEqual(<<"**fet** och *kursiv* och ~stryk~">>,
                 toMD(<<"*fet* och _kursiv_ och ~stryk~">>)),
    ?assertEqual(<<"snake_case_name och 2*3*4">>,
                 toMD(<<"snake_case_name och 2*3*4">>)),
    ?assertEqual(<<"> citat\n<tag> & mer">>,
                 toMD(<<"&gt; citat\n&lt;tag&gt; &amp; mer">>)).

slack_refs() ->
    ?assertEqual(<<"Hej @Anna och @U9 i #general och #random">>,
                 toMD(<<"Hej <@U1> och <@U9> i <#C1> och <#C2|random>">>)),
    ?assertEqual(<<"[länk & mer](https://x.se/a?b=1&c=2) och "
                   "[https://y.se](https://y.se)"/utf8>>,
                 toMD(<<"<https://x.se/a?b=1&amp;c=2|länk &amp; mer> och "
                        "<https://y.se>"/utf8>>)),
    ?assertEqual(<<"@here @team Feb 18, 2014">>,
                 toMD(<<"<!here> <!subteam^S1|@team> "
                        "<!date^1392734382^{date}|Feb 18, 2014>">>)).

slack_code() ->
    ?assertEqual(<<"kod `*inte fet*` och \n```\nx *y*\n```\n">>,
                 toMD(<<"kod `*inte fet*` och ```x *y*```">>)),
    ?assertEqual(<<"oavslutad `kod">>, toMD(<<"oavslutad `kod">>)).

slack_emoji() ->
    ?assertEqual(<<"🎉🏻 :okänd: 10:30:45"/utf8>>,
                 toMD(<<":tada::skin-tone-2: :okänd: 10:30:45"/utf8>>)).

md_to_slack() ->
    ?assertEqual(<<"*fet* och _kursiv_ och _också_ ~stryk~"/utf8>>,
                 plugin_slack:mdToSlack(<<"**fet** och *kursiv* och _också_ "
                                          "~~stryk~~"/utf8>>)),
    ?assertEqual(<<"*Rubrik*\n<https://x.se?a=1&b=2|länk> "
                   "<https://x.se/i.png|bild>"/utf8>>,
                 plugin_slack:mdToSlack(<<"# Rubrik\n[länk](https://x.se?a=1&b=2) "
                                          "![bild](https://x.se/i.png)"/utf8>>)),
    ?assertEqual(<<"snake_case och 2*3*4">>,
                 plugin_slack:mdToSlack(<<"snake_case och 2*3*4">>)).

md_to_slack_code() ->
    ?assertEqual(<<"a &lt; b &amp; c `x &lt; y` ```\n&lt;p&gt;\n```">>,
                 plugin_slack:mdToSlack(<<"a < b & c `x < y` ```\n<p>\n```">>)).

md_to_slack_bad_utf8() ->
    ?assertEqual(<<"a &lt; ", 229>>, plugin_slack:mdToSlack(<<"a < ", 229>>)).

status() ->
    ?assertEqual(#{status_text => <<"Möte"/utf8>>,
                   status_emoji => <<":no_entry:">>},
                 plugin_slack:profile("Busy", "Möte", "Available")),
    ?assertEqual(#{status_text => <<"Lunch">>, status_emoji => <<>>},
                 plugin_slack:profile("Available", "Lunch", "Busy")),
    ?assertEqual(#{status_text => <<"Lunch">>},
                 plugin_slack:profile("Away", "Lunch", "Available")),
    ?assertEqual(#{status_text => <<>>, status_emoji => <<>>},
                 plugin_slack:profile("Available", "", "Away")),
    ?assertEqual(100, string:length(plugin_slack:statusText(
                                      lists:duplicate(150, $x)))).

dnd() ->
    Now = erlang:system_time(second),
    ?assert(plugin_slack:inDnd(#{<<"snooze_enabled">> => true})),
    ?assert(plugin_slack:inDnd(#{<<"dnd_enabled">> => true,
                                 <<"next_dnd_start_ts">> => Now - 60,
                                 <<"next_dnd_end_ts">> => Now + 60})),
    ?assertNot(plugin_slack:inDnd(#{<<"dnd_enabled">> => true,
                                    <<"next_dnd_start_ts">> => Now + 60,
                                    <<"next_dnd_end_ts">> => Now + 120})),
    ?assertNot(plugin_slack:inDnd(#{<<"snooze_enabled">> => false})).

file_name() ->
    ?assertEqual("a_b_c.png",
                 plugin_slack:fileName(#{<<"name">> => <<"a/b\\c.png">>})),
    ?assertEqual("hidden",
                 plugin_slack:fileName(#{<<"name">> => <<"..hidden">>})),
    ?assertEqual("file", plugin_slack:fileName(#{})).
