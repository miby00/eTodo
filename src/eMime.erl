%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2017 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eMime).
-author("mikael.bylund@gmail.com").

%% API
-export([constructMail/4]).

-import(eHtml, [aTag/2,    headTag/1,  titleTag/1,
                metaTag/1, styleTag/2, bodyTag/1]).

-import(eTodoUtils, [getStyleSheet/1]).

-define(LF, "\r\n").

constructMail(UserName, Subject, From, To) ->
    [
     "From: ", From, ?LF,
     "Content-Type: text/html; charset=\"UTF-8\"", ?LF,
     "Content-Transfer-Encoding: quoted-printable", ?LF,
     "Subject: ", Subject, ?LF,
     "To: ", To, ?LF,
     "MIME-Version: 1.0", ?LF,
     ?LF,
     body(UserName),
     ?LF, ".", ?LF
    ].

body(UserName) ->
    StyleSheet   = getStyleSheet(UserName),
    {ok, Styles} = file:read_file(StyleSheet),
    Messages     = eWeb:getMessages(),
    ["<!DOCTYPE html><html>",
     [headTag(
        [titleTag(["eTodo - ", UserName]),
         metaTag([{"http-equiv", "Content-Type"},
                  {content, "text/html; charset=UTF-8"}]),
         styleTag([{type, "text/css"}], Styles)
        ]),
      bodyTag(Messages)]].