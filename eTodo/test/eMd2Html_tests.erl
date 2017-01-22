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
    ?assert(Result == <<"<p>Header 2\r\n</p><ul><li>Test</li></ul>">>).

ollist_1_test() ->
    Result = eMd2Html:convert(<<"Header 2\r\n1. Test">>),
    ?assert(Result == <<"<p>Header 2\r\n</p><ol start='1'><li>Test</li></ol>">>).

paragraph_1_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text">>),
    ?assert(Result == <<"<p>Hej här kommer lite text</p>">>).

paragraph_2_test() ->
    Result = eMd2Html:convert(<<"Hej här kommer lite text\r\nMed två rader">>),
    ?assert(Result == <<"<p>Hej här kommer lite text\r\nMed två rader</p>">>).
