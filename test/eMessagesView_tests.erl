-module(eMessagesView_tests).

-include_lib("eunit/include/eunit.hrl").

-define(icons, "/opt/etodo/priv/Icons/").

legacy_chat(Body) ->
    iolist_to_binary(
      ["<table align='left' width='100%'><tr><td><img src='", ?icons,
       "userChat.png' /></td><td><font size=3 face='Impact, Charcoal, "
       "sans-serif'>17:34:09</font></td><td><font size=3 face='Impact, "
       "Charcoal, sans-serif'>Ann to miby00</font></td></tr><tr><td></td>"
       "<td><img src='", ?icons, "portrait.png' /></td><td><font "
       "color='Black' size=3 face='Tahoma, Geneva, sans-serif'>", Body,
       "</font></td></tr></table>"]).

legacy_system(Uid, Body) ->
    Link = base64:encode(term_to_binary({uid, Uid})),
    iolist_to_binary(
      ["<table><tr><td><font face='Impact, Charcoal, sans-serif'>15:11:19"
       "</font></td><td><font face='Impact, Charcoal, sans-serif'>"
       "<a href='", Link, "'>eTodo</a></font></td></tr><tr><td>"
       "<font face='Tahoma, Geneva, sans-serif'>", Body,
       "</font></td></tr></table>"]).

html(Nodes) -> iolist_to_binary(mochiweb_html:to_html({<<"div">>, [], Nodes})).

has(Needle, Haystack) -> binary:match(Haystack, Needle) =/= nomatch.

parse_chat_test() ->
    {Title, Link, Body} =
        eMessagesView:parse_legacy(legacy_chat(<<"<p>Ett svar</p>">>)),
    ?assertEqual(<<"Ann to miby00">>, Title),
    ?assertEqual(blocked, Link),
    ?assertEqual([{<<"p">>, [], [<<"Ett svar">>]}], Body).

parse_system_link_test() ->
    {Title, Link, _Body} =
        eMessagesView:parse_legacy(legacy_system(7, <<"<p>Moved</p>">>)),
    ?assertEqual(<<"eTodo">>, Title),
    ?assertEqual({ok, <<"/work-log/action/task/7">>}, Link).

sanitize_test() ->
    Body = <<"<p onclick='x()' style='color:red'>Hi <b>there</b>"
             "<script>alert(1)</script><style>p{}</style>"
             "<meta http-equiv='refresh' content='0;url=https://evil'>"
             "<a href='javascript:alert(1)'>bad</a> "
             "<a href='https://example.com/a?b=1'>good</a> "
             "<img class='emote' alt=':)' src='/x/Icons/happy.png'>"
             "<img src='https://example.com/cat.png' alt='cat'>"
             "<img src='/etc/secret.png'>"
             "<iframe src='https://evil'></iframe></p>">>,
    {_, _, Nodes} = eMessagesView:parse_legacy(legacy_chat(Body)),
    Html = html(Nodes),
    ?assertNot(has(<<"script">>, Html)),
    ?assertNot(has(<<"alert">>, Html)),
    ?assertNot(has(<<"onclick">>, Html)),
    ?assertNot(has(<<"style">>, Html)),
    ?assertNot(has(<<"refresh">>, Html)),
    ?assertNot(has(<<"iframe">>, Html)),
    ?assertNot(has(<<"javascript">>, Html)),
    ?assertNot(has(<<"/etc/secret">>, Html)),
    ?assertNot(has(<<"<img">>, Html)),
    ?assert(has(<<"<b>there</b>">>, Html)),
    ?assert(has(<<"bad">>, Html)),
    ?assert(has(<<"href=\"https://example.com/a?b=1\"">>, Html)),
    ?assert(has(<<"rel=\"noopener noreferrer\"">>, Html)),
    ?assert(has(<<"class=\"emoji\"">>, Html)),
    ?assert(has(<<"🙂"/utf8>>, Html)),
    ?assert(has(<<"href=\"https://example.com/cat.png\"">>, Html)).

document_test() ->
    Now = os:timestamp(),
    {Mega, Sec, Micro} = Now,
    Later = {Mega, Sec + 60, Micro},
    Records =
        [{Now, msgEntry, "Ann", ["me"],
          legacy_chat(<<"<p>Hello</p>">>)},
         {Later, msgEntry, "Ann", ["me"],
          legacy_chat(<<"<p><img class='emote' alt='lol' "
                        "src='/p/Icons/lol.png'></p>">>)},
         {Later, msgEntry, "me", ["Ann", "Bob"],
          legacy_chat(<<"<p>Hi both</p>">>)}],
    Theme = #{background => <<"#171717">>, foreground => <<"#EEEEEE">>,
              accent => <<"#0088FF">>},
    Page = eMessagesView:document(<<"me">>, chat, Records, <<>>, Theme),
    ?assert(has(<<"data-bs-theme=\"dark\"">>, Page)),
    ?assert(has(<<"img-src 'self'">>, Page)),
    ?assert(has(<<"class=\"day\"">>, Page)),
    ?assert(has(<<"Today">>, Page)),
    ?assert(has(<<"class=\"msg received\"">>, Page)),
    ?assert(has(<<"class=\"msg received continued\"">>, Page)),
    ?assert(has(<<"class=\"msg sent\"">>, Page)),
    ?assert(has(<<"to Ann, Bob">>, Page)),
    ?assert(has(<<"bubble jumbo">>, Page)),
    ?assert(has(<<"/messages/action/menu">>, Page)),
    ?assert(has(<<"id=\"end\"">>, Page)),
    ?assertNot(has(<<"report-header">>, Page)).

browser_document_test() ->
    Records = [{os:timestamp(), systemEntry, "", [],
                legacy_system(9, <<"<p>See <a href='https://x.test'>x</a>"
                                   "</p>">>)}],
    Page = eMessagesView:document(<<"me">>, system, Records, <<>>, #{}),
    ?assert(has(<<"report-header">>, Page)),
    ?assert(has(<<"card-msg system task">>, Page)),
    ?assert(has(<<"/work-log/action/task/9">>, Page)),
    ?assert(has(<<"target=\"_blank\"">>, Page)),
    ?assertNot(has(<<"menu-button">>, Page)).

empty_document_test() ->
    Page = eMessagesView:document(<<"me">>, reminders, [], <<>>, #{}),
    ?assert(has(<<"No reminders">>, Page)).

browser_panels_test() ->
    Panels = [{chat, []}, {reminders, []}, {system, []}],
    Page = eMessagesView:browser_document(<<"me">>, reminders, Panels, <<>>),
    ?assert(has(<<"class=\"panels\"">>, Page)),
    ?assert(has(<<"data-selected=\"reminders\"">>, Page)),
    [?assert(has(<<"id=\"", Id/binary, "\"">>, Page))
     || Id <- [<<"chat">>, <<"reminders">>, <<"system">>]],
    ?assert(has(<<"href=\"#system\"">>, Page)),
    ?assert(has(<<"script-src 'self'">>, Page)),
    ?assert(has(<<"/assets/etodo.js">>, Page)),
    ?assert(has(<<"data-swipe-prev=\"/\"">>, Page)).

embedded_has_no_script_test() ->
    Theme = #{background => <<"#ffffff">>, foreground => <<"#000000">>,
              accent => <<"#0088FF">>},
    Page = eMessagesView:document(<<"me">>, chat, [], <<>>, Theme),
    ?assertNot(has(<<"<script">>, Page)),
    ?assertNot(has(<<"script-src">>, Page)).

repeated_cards_collapse_test() ->
    Html = legacy_system(3, <<"<p>Break time</p>">>),
    Records = [{os:timestamp(), alarmEntry, "", [], Html},
               {os:timestamp(), alarmEntry, "", [], Html},
               {os:timestamp(), alarmEntry, "", [], Html}],
    Page = eMessagesView:document(<<"me">>, reminders, Records, <<>>, #{}),
    ?assert(has(<<"×3"/utf8>>, Page)),
    ?assertEqual(1, length(binary:matches(Page, <<"Break time">>))).

params_test() ->
    ?assertEqual(chat, eMessagesView:kind(<<"bogus">>)),
    ?assertEqual(system, eMessagesView:kind(<<"system">>)),
    ?assertEqual(all, eMessagesView:filter(<<"all">>, <<"Ann">>)),
    ?assertEqual(["Ann", "Bob"],
                 eMessagesView:filter(<<"peers">>, <<"Ann\nBob">>)),
    ?assertEqual([], eMessagesView:filter(<<"peers">>, <<>>)).

avatar_rejects_traversal_test() ->
    ?assertMatch({error, _}, eMessagesView:avatar(<<"../../etc/passwd">>)),
    ?assertMatch({error, _}, eMessagesView:avatar(<<"a/b">>)),
    ?assertMatch({error, _}, eMessagesView:avatar(<<>>)).
