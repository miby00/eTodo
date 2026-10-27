-module(eHtml_tests).

-include_lib("eunit/include/eunit.hrl").

message_theme_test() ->
    Html = <<"<table width=100%><tr><td bgcolor='#E0E0E0'>"
             "<font color='Black'>Message</font>"
             "<font color='#303030'>Heading</font>"
             "<font color='Red'>Reminder</font></td></tr></table>">>,
    Page = theme(Html),
    [Body] = elements(<<"body">>, Page),
    {_, BodyAttributes, _} = Body,
    ?assertEqual(<<"#171717">>, value(<<"bgcolor">>, BodyAttributes)),
    ?assertEqual(<<"#EEEEEE">>, value(<<"text">>, BodyAttributes)),
    ?assertEqual(<<"#66AAFF">>, value(<<"link">>, BodyAttributes)),
    [{_, CellAttributes, _}] = elements(<<"td">>, Page),
    ?assertEqual(<<"#242424">>, value(<<"bgcolor">>, CellAttributes)),
    Colors = [value(<<"color">>, Attributes)
              || {_, Attributes, _} <- elements(<<"font">>, Page)],
    ?assertEqual([<<"#EEEEEE">>, <<"#EEEEEE">>, <<"Red">>], Colors).

message_content_test() ->
    Text = unicode:characters_to_binary(["Hej ", 16#E5, " & color='Black'"]),
    Html = ["<p>Hej ", 16#E5, " &amp; color='Black'</p>",
            "<a href='https://example.org/?a=1&amp;b=2'>Link</a>",
            "<img src='/portrait.png'>"],
    Page = theme(Html),
    [{_, _, [Text]}] = elements(<<"p">>, Page),
    [{_, LinkAttributes, _}] = elements(<<"a">>, Page),
    ?assertEqual(<<"https://example.org/?a=1&b=2">>,
                 value(<<"href">>, LinkAttributes)),
    [{_, ImageAttributes, _}] = elements(<<"img">>, Page),
    ?assertEqual(<<"/portrait.png">>, value(<<"src">>, ImageAttributes)).

empty_messages_test() ->
    [{_, _, []}] = elements(<<"body">>, theme([])).

light_messages_test() ->
    Html = <<"<font color='Black'>Message</font>">>,
    Page = mochiweb_html:parse(
             eHtml:themeMessages(Html, <<"#FFFFFF">>, <<"#000000">>,
                                  <<"#F0F0F0">>, <<"#0000FF">>)),
    [{_, Attributes, _}] = elements(<<"font">>, Page),
    ?assertEqual(<<"#000000">>, value(<<"color">>, Attributes)).

theme(Html) ->
    mochiweb_html:parse(
      eHtml:themeMessages(Html, <<"#171717">>, <<"#EEEEEE">>,
                           <<"#242424">>, <<"#66AAFF">>)).

value(Name, Attributes) -> proplists:get_value(Name, Attributes).

elements(Tag, {Name, _Attributes, Children} = Node) ->
    Current = case Name =:= Tag of true -> [Node]; false -> [] end,
    Current ++ lists:append([elements(Tag, Child) || Child <- Children]);
elements(_Tag, _Node) -> [].