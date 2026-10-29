-module(eGuiFunctions_tests).

-include_lib("eunit/include/eunit.hrl").

preview_control_types_test() ->
    ?assertEqual(wxWebView, eGuiFunctions:type("descAreaPreview")),
    ?assertEqual(wxWebView, eGuiFunctions:type("commentAreaPreview")).
