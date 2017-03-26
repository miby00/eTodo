%%%-------------------------------------------------------------------
%%% @author    Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jan 2017 13:30
%%%-------------------------------------------------------------------
-module(eMd2Html_tests).
-author("mikael.bylund@gmail.com").

-include_lib("eunit/include/eunit.hrl").

header_1_test() ->
    Result = eMd2Html:convert(<<"# Header 1">>),
    Expect = <<"<h1>Header 1</h1>">>,
    ?assertEqual(Expect, Result).

header_2_test() ->
    Result = eMd2Html:convert(<<"# Header 1 #">>),
    Expect = <<"<h1>Header 1</h1>">>,
    ?assertEqual(Expect, Result).

header_3_test() ->
    Result = eMd2Html:convert(<<"## Header 2">>),
    Expect = <<"<h2>Header 2</h2>">>,
    ?assertEqual(Expect, Result).

header_4_test() ->
    Result = eMd2Html:convert(<<"## Header 2 ####">>),
    Expect = <<"<h2>Header 2</h2>">>,
    ?assertEqual(Expect, Result).

header_5_test() ->
    Result = eMd2Html:convert(<<"Header 1\r\n=">>),
    Expect = <<"<h1>Header 1</h1>">>,
    ?assertEqual(Expect, Result).

header_6_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n-">>),
    Expect = <<"<h2>Header 2</h2>">>,
    ?assertEqual(Expect, Result).

header_7_test() ->
    Result = eMd2Html:convert(<<"### Header 3 #####">>),
    Expect = <<"<h3>Header 3</h3>">>,
    ?assertEqual(Expect, Result).

header_8_test() ->
    Result = eMd2Html:convert(<<"#### Header 4 #">>),
    Expect = <<"<h4>Header 4</h4>">>,
    ?assertEqual(Expect, Result).

header_9_test() ->
    Result = eMd2Html:convert(<<"##### Header 5 #####">>),
    Expect = <<"<h5>Header 5</h5>">>,
    ?assertEqual(Expect, Result).

header_10_test() ->
    Result = eMd2Html:convert(<<"###### Header 6">>),
    Expect = <<"<h6>Header 6</h6>">>,
    ?assertEqual(Expect, Result).

header_11_test() ->
    Result = eMd2Html:convert(<<"Header 1\r\n========">>),
    Expect = <<"<h1>Header 1</h1>">>,
    ?assertEqual(Expect, Result).

header_12_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n--------">>),
    Expect = <<"<h2>Header 2</h2>">>,
    ?assertEqual(Expect, Result).

header_13_test() ->
    Result = eMd2Html:convert(<<"Header 1\r\n=\r\n">>),
    Expect = <<"<h1>Header 1</h1>">>,
    ?assertEqual(Expect, Result).

ullist_1_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n- Test">>),
    Expect = <<"<p>Header 2</p><ul><li>Test</li></ul>">>,
    ?assertEqual(Expect, Result).

ullist_2_test() ->
    Result = eMd2Html:convert(<<"- Test1\r\n  - Test2">>),
    Expect = <<"<ul><li>Test1</li><ul><li>Test2</li></ul></ul>">>,
    ?assertEqual(Expect, Result).

ullist_3_test() ->
    Result = eMd2Html:convert(<<"* Test1\r\n"
                                "  * Test2\r\n"
                                "    * Test3\r\n"
                                "* Test4\r\n">>),
    Expect = <<"<ul><li>Test1</li>"
               "<ul><li>Test2</li>"
               "<ul><li>Test3</li></ul></ul>"
               "<li>Test4</li></ul>">>,
    ?assertEqual(Expect, Result).

ullist_4_test() ->
    Result = eMd2Html:convert(<<"* Test1\r\n"
                                "  Test2\r\n"
                                "  Test3\r\n">>),
    Expect = <<"<ul><li>Test1\r\nTest2\r\nTest3</li></ul>">>,
    ?assertEqual(Expect, Result).

ullist_5_test() ->
    Result = eMd2Html:convert(<<"1. Test1\r\n  \r\n### Test3 ###">>),
    Expect = <<"<ol start='1'><li>Test1</li></ol><h3>Test3</h3>">>,
    ?assertEqual(Expect, Result).

ullist_6_test() ->
    Result = eMd2Html:convert(<<"* Test1\r\n  \r\n### Test3 ###">>),
    Expect = <<"<ul><li>Test1</li></ul><h3>Test3</h3>">>,
    ?assertEqual(Expect, Result).

ullist_7_test() ->
    Result = eMd2Html:convert(<<"* Test1\r\n\r\n   Test3">>),
    Expect = <<"<ul><li>Test1</li></ul><p>Test3</p>">>,
    ?assertEqual(Expect, Result).

ullist_8_test() ->
    Result = eMd2Html:convert(<<"Hej * Test1 Test3">>),
    Expect = <<"<p>Hej * Test1 Test3</p>">>,
    ?assertEqual(Expect, Result).

ollist_1_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n1. Test">>),
    Expect = <<"<p>Header 2</p><ol start='1'><li>Test</li></ol>">>,
    ?assertEqual(Expect, Result).

ollist_2_test() ->
    Result = eMd2Html:convert(<<"1. Test1\r\n   2. Test2">>),
    Expect = <<"<ol start='1'><li>Test1</li>"
               "<ol start='2'><li>Test2</li></ol></ol>">>,
    ?assertEqual(Expect, Result).

ollist_3_test() ->
    Result = eMd2Html:convert(<<"1. Test1\r\n   * Test2">>),
    Expect = <<"<ol start='1'><li>Test1</li>"
               "<ul><li>Test2</li></ul></ol>">>,
    ?assertEqual(Expect, Result).

ollist_4_test() ->
    Result = eMd2Html:convert(<<"1. Test1\r\n"
                                "   Test2\r\n"
                                "   Test3\r\n">>),
    Expect = <<"<ol start='1'><li>Test1\r\nTest2\r\nTest3</li></ol>">>,
    ?assertEqual(Expect, Result).

paragraph_1_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text">>),
    Expect = <<"<p>Hej här kommer lite text</p>">>,
    ?assertEqual(Expect, Result).

paragraph_2_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text\r\nMed två rader">>),
    Expect = <<"<p>Hej här kommer lite text\r\nMed två rader</p>">>,
    ?assertEqual(Expect, Result).

paragraph_3_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text  \r\n"
                                "Med två rader">>),
    Expect = <<"<p>Hej här kommer lite text<br />\r\n"
               "Med två rader</p>">>,
    ?assertEqual(Expect, Result).

paragraph_4_test() ->
    Result = eMd2Html:convert(<<"Hej\r\nSvejs  \r\n">>),
    Expect = <<"<p>Hej\r\nSvejs</p>">>,
    ?assertEqual(Expect, Result).

url_1_test() ->
    Result = eMd2Html:convert(<<"<http://google.com>">>),
    Expect = <<"<p><a href='http://google.com'>http://google.com</a></p>">>,
    ?assertEqual(Expect, Result).

url_2_test() ->
    Result = eMd2Html:convert(<<"x<http://google.com>y<http://google.com>">>),
    Expect = <<"<p>x<a href='http://google.com'>http://google.com</a>"
               "y<a href='http://google.com'>http://google.com</a></p>">>,
    ?assertEqual(Expect, Result).

blockquote_1_test() ->
    Result = eMd2Html:convert(<<"> Test">>),
    Expect = <<"<blockquote><p>Test</p></blockquote>">>,
    ?assertEqual(Expect, Result).

blockquote_2_test() ->
    Result = eMd2Html:convert(<<"> Test\r\n">>),
    Expect = <<"<blockquote><p>Test</p></blockquote>">>,
    ?assertEqual(Expect, Result).

blockquote_3_test() ->
    Result = eMd2Html:convert(<<"> Test1\r\n> Test2\r\n">>),
    Expect = <<"<blockquote><p>Test1\r\nTest2</p></blockquote>">>,
    ?assertEqual(Expect, Result).

blockquote_4_test() ->
    Result = eMd2Html:convert(<<"> Test\r\n> av\r\n> Block quote\r\n">>),
    Expect = <<"<blockquote><p>Test\r\nav\r\nBlock quote</p></blockquote>">>,
    ?assertEqual(Expect, Result).

blockquote_5_test() ->
    Result = eMd2Html:convert(<<"Hej\r\n\r\n> Test\r\n> av\r\n> Block quote\r\n">>),
    Expect = <<"<p>Hej</p>"
                 "<blockquote>"
                 "<p>Test\r\nav\r\nBlock quote</p>"
                 "</blockquote>">>,
    ?assertEqual(Expect, Result).

code_1_test() ->
    Result = eMd2Html:convert(<<"    Hej\r\n">>),
    Expect = <<"<pre><code>Hej</code></pre>">>,
    ?assertEqual(Expect, Result).

code_2_test() ->
    Result = eMd2Html:convert(<<"    Hej\r\n    Svejs\r\n">>),
    Expect = <<"<pre><code>Hej\r\nSvejs</code></pre>">>,
    ?assertEqual(Expect, Result).

fcode_1_test() ->
    Result = eMd2Html:convert(<<"```\r\nTest">>),
    Expect = <<"<pre><code>Test</code></pre>">>,
    ?assertEqual(Expect, Result).

fcode_2_test() ->
    Result = eMd2Html:convert(<<"```\r\nTest\r\n```">>),
   Expect = <<"<pre><code>Test\r\n</code></pre>">>,
    ?assertEqual(Expect, Result).

fcode_3_test() ->
    Result = eMd2Html:convert(<<"~~~\r\nTest\r\n~~~">>),
    Expect = <<"<pre><code>Test\r\n</code></pre>">>,
    ?assertEqual(Expect, Result).

smiley_1_test() ->
    Result = eMd2Html:convert(<<":-):(">>),
    {match, Matches} = re:run(Result, "img class='emote'", [global]),
    ?assertEqual(2, length(Matches)).

smiley_2_test() ->
    Result = eMd2Html:convert(<<":-)<3">>),
    {match, Matches} = re:run(Result, "img class='emote'", [global]),
    ?assertEqual(2, length(Matches)).
