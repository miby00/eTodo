-module(eGuiFunctions_tests).

-include_lib("eunit/include/eunit.hrl").

preview_control_types_test() ->
    ?assertEqual(wxWebView, eGuiFunctions:type("descAreaPreview")),
    ?assertEqual(wxWebView, eGuiFunctions:type("commentAreaPreview")).

list_column_strips_markdown_test() ->
    ?assertEqual("Title", eGuiFunctions:colFormat("## Title\n\nBody")),
    ?assertEqual("Fix bold and code in docs and more",
                 eGuiFunctions:colFormat(
                   "Fix **bold** and `code` in [docs](https://x.se) "
                   "and *more*")),
    ?assertEqual("a_b_c snake_case 2*3*4 *literal*",
                 eGuiFunctions:colFormat(
                   "a_b_c snake_case 2*3*4 \\*literal\\*")),
    ?assertEqual("Räksmörgås logo quoted",
                 eGuiFunctions:colFormat(
                   "~~Räksmörgås~~ ![logo](x.png)\n> quoted")),
    ?assertEqual("Long" ++ lists:duplicate(156, $x) ++ "...",
                 eGuiFunctions:colFormat(
                   "**Long**" ++ lists:duplicate(200, $x))).
