%%%-------------------------------------------------------------------
%%% @author    Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Dec 2017 07:31
%%%-------------------------------------------------------------------
-module(eWebPlugin).
-author("Mikael Bylund <mikael.bylund@gmail.com>").

%% API
-export([slackAuth/3, slackCallback/3]).

-import(eHtml, [htmlTag/0,   htmlTag/1,   htmlTag/2,
                headTag/0,   headTag/1,   headTag/2,
                bodyTag/0,   bodyTag/1,   bodyTag/2,
                titleTag/1,  titleTag/2,
                styleTag/1,  styleTag/2,
                tableTag/0,  tableTag/1,  tableTag/2,
                divTag/0,    divTag/1,    divTag/2,
                fontTag/0,   fontTag/1,   fontTag/2,
                pTag/0,      pTag/1,      pTag/2,
                bTag/0,      bTag/1,      bTag/2,
                tdTag/0,     tdTag/1,     tdTag/2,
                trTag/0,     trTag/1,     trTag/2,
                brTag/0,
                formTag/0,   formTag/1,   formTag/2,
                aTag/0,      aTag/1,      aTag/2,
                selectTag/1, selectTag/2, inputTag/1,
                metaTag/1,   imgTag/1]).

slackAuth(SessionId, _Env, _Input) ->
    Cid  = application:get_env(eTodo, slackClientId,     ""),
    Redirect = "https://slack.com/oauth/authorize?client_id=" ++
        Cid ++ "&scope=client",
    HtmlPage = ["location: ", Redirect, "\r\n\r\n"],
    mod_esi:deliver(SessionId, HtmlPage).

slackCallback(SessionId, Env, Input) ->
    HtmlPage = doSlackCallback(SessionId, Env, Input),
    mod_esi:deliver(SessionId, HtmlPage).

doSlackCallback(_SessioId, _Env, Input) ->
    Cid  = application:get_env(eTodo, slackClientId,     ""),
    CSec = application:get_env(eTodo, slackClientSecret, ""),

    KeyValueList = httpd:parse_query(Input),
    QueryParams  = maps:from_list(KeyValueList),
    Code         = maps:get("code", QueryParams, ""),
    Url  = "https://slack.com/api/oauth.access?client_id=" ++
           Cid ++ "&client_secret=" ++ CSec ++ "&code=" ++ Code,

    case httpc:request(get, {Url, ""}, [],[{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, JSON}} ->
            JMap = jsx:decode(JSON, [return_maps]),
            [headTag(), bodyTag(pTag(maps:get(<<"access_token">>, JMap, JSON)))];
        _ ->
            [headTag(), bodyTag(pTag("Call failed"))]
    end.