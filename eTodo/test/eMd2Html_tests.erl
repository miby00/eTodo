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
    ?assert(Result == <<"<h1>Header 1</h1>">>).

header_2_test() ->
    Result = eMd2Html:convert(<<"# Header 1 #">>),
    ?assert(Result == <<"<h1>Header 1</h1>">>).

header_3_test() ->
    Result = eMd2Html:convert(<<"## Header 2">>),
    ?assert(Result == <<"<h2>Header 2</h2>">>).

header_4_test() ->
    Result = eMd2Html:convert(<<"## Header 2 ####">>),
    ?assert(Result == <<"<h2>Header 2</h2>">>).

header_5_test() ->
    Result = eMd2Html:convert(<<"Header 1\r\n=">>),
    ?assert(Result == <<"<h1>Header 1</h1>">>).

header_6_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n-">>),
    ?assert(Result == <<"<h2>Header 2</h2>">>).

header_7_test() ->
    Result = eMd2Html:convert(<<"### Header 3 #####">>),
    ?assert(Result == <<"<h3>Header 3</h3>">>).

header_8_test() ->
    Result = eMd2Html:convert(<<"#### Header 4 #">>),
    ?assert(Result == <<"<h4>Header 4</h4>">>).

header_9_test() ->
    Result = eMd2Html:convert(<<"##### Header 5 #####">>),
    ?assert(Result == <<"<h5>Header 5</h5>">>).

header_10_test() ->
    Result = eMd2Html:convert(<<"###### Header 6">>),
    ?assert(Result == <<"<h6>Header 6</h6>">>).

header_11_test() ->
    Result = eMd2Html:convert(<<"Header 1\r\n========">>),
    ?assert(Result == <<"<h1>Header 1</h1>">>).

header_12_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n--------">>),
    ?assert(Result == <<"<h2>Header 2</h2>">>).

ullist_1_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n- Test">>),
    ?assert(Result == <<"<p>Header 2</p><ul><li>Test</li></ul>">>).

ullist_2_test() ->
    Result = eMd2Html:convert(<<"- Test1\r\n  - Test2">>),
    ?assert(Result == <<"<ul><li>Test1</li><ul><li>Test2</li></ul></ul>">>).

ullist_3_test() ->
    Result = eMd2Html:convert(<<"* Test1\r\n"
                                "  * Test2\r\n"
                                "    * Test3\r\n"
                                "* Test4\r\n">>),
    ?assert(Result == <<"<ul><li>Test1</li>"
                        "<ul><li>Test2</li>"
                        "<ul><li>Test3</li></ul></ul>"
                        "<li>Test4</li></ul>">>).

ollist_1_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n1. Test">>),
    ?assert(Result == <<"<p>Header 2</p><ol start='1'><li>Test</li></ol>">>).

ollist_2_test() ->
    Result = eMd2Html:convert(<<"1. Test1\r\n   2. Test2">>),
    ?assert(Result == <<"<ol start='1'><li>Test1</li>"
                        "<ol start='2'><li>Test2</li></ol></ol>">>).

ollist_3_test() ->
    Result = eMd2Html:convert(<<"1. Test1\r\n   * Test2">>),
    ?assert(Result == <<"<ol start='1'><li>Test1</li>"
                        "<ul><li>Test2</li></ul></ol>">>).

paragraph_1_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text">>),
    ?assert(Result == <<"<p>Hej här kommer lite text</p>">>).

paragraph_2_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text\r\nMed två rader">>),
    ?assert(Result == <<"<p>Hej här kommer lite text\r\nMed två rader</p>">>).

paragraph_3_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text  \r\n"
                                "Med två rader">>),
    ?assert(Result == <<"<p>Hej här kommer lite text<br />\r\n"
                        "Med två rader</p>">>).

paragraph_4_test() ->
    Result = eMd2Html:convert(<<"Hej\r\nSvejs  \r\n">>),
    ?assert(Result == <<"<p>Hej\r\nSvejs</p>">>).

url_1_test() ->
    Result = eMd2Html:convert(<<"<http://google.com>">>),
    ?assert(Result == <<"<p><a href='http://google.com'>http://google.com</a></p>">>).

url_2_test() ->
    Result = eMd2Html:convert(<<"x<http://google.com>y<http://google.com>">>),
    ?assert(Result ==
            <<"<p>x<a href='http://google.com'>http://google.com</a>"
              "y<a href='http://google.com'>http://google.com</a></p>">>).

