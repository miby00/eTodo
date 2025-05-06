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
-export([constructMail/4, constructMail/6]).

-import(eHtml, [aTag/2,    headTag/1,  titleTag/1,
                metaTag/1, styleTag/2, bodyTag/1]).

-import(eTodoUtils, [getStyleSheet/1, getRootDir/0]).

-define(LF, "\r\n").
-define(BDDef, "----=_Part_710804_24963257.1485877260241").
-define(BD,    "------=_Part_710804_24963257.1485877260241").
-define(BDEnd, "------=_Part_710804_24963257.1485877260241--").

%%--------------------------------------------------------------------
%% @doc
%% Construct notification mail.
%%
%% @end
%%--------------------------------------------------------------------
constructMail(UserName, Subject, ReplyTo, To) ->
    Messages = iolist_to_binary(eWeb:getMessages()),
    constructMail(UserName, Subject, To ,ReplyTo, To, Messages).

constructMail(UserName, Subject, From, From, To, Messages) ->
    HeaderStart = [
                   "From: ", From, ?LF
                  ],
    doConstructMail(HeaderStart, UserName, Subject, To, Messages);
constructMail(UserName, Subject, From, ReplyTo, To, Messages) ->
    HeaderStart = [
                   "From: ", From, ?LF,
                   "Reply-To: ", ReplyTo, ?LF
                  ],
    doConstructMail(HeaderStart, UserName, Subject, To, Messages).

doConstructMail(HeaderStart, UserName, Subject, To, Messages) ->
    HeaderStart ++
        [
         "Content-Type: multipart/related;", ?LF, 9,
         "boundary=\"", ?BDDef, "\"", ?LF,
         "Subject: ", Subject, ?LF,
         "To: ", makeAddressHeader(To), ?LF,
         "MIME-Version: 1.0", ?LF,
         ?LF,
         ?BD, ?LF,
         "Content-Type: text/html; charset=\"UTF-8\"", ?LF,
         "Content-Transfer-Encoding: base64", ?LF,
         ?LF,
         body(UserName, Messages), ?LF,
         addMimeParts(Messages),
         ?LF, ?BDEnd, ?LF,
         ?LF, ".", ?LF
        ].

body(UserName, Messages) ->
    StyleSheet   = filename:join([getRootDir(), "css", "mail.css"]),
    {ok, Styles} = file:read_file(StyleSheet),
    Messages2    = replaceParts(Messages, <<>>),
    encodeBase64(
      iolist_to_binary(["<!DOCTYPE html><html>",
                        [headTag(
                           [titleTag(["eTodo - ", UserName]),
                            metaTag([{"http-equiv", "Content-Type"},
                                     {content, "text/html; charset=UTF-8"}]),
                            styleTag([{type, "text/css"}], Styles)
                           ]),
                         bodyTag([Messages2])]])).

addMimeParts(Messages) ->
    Parts = findParts(Messages, []),
    [makeMimePart(File) || File <- Parts].

findParts(<<>>, Acc) ->
    Acc;
findParts(<<"<img src='", Rest/binary>>, Acc) ->
    checkUrl(Rest, Acc);
findParts(<<"' src='", Rest/binary>>, Acc) ->
    checkUrl(Rest, Acc);
findParts(<<_:8, Rest/binary>>, Acc) ->
    findParts(Rest, Acc).

checkUrl(Rest, Acc) ->
    case getFileName(Rest) of
        {true, FileName, Rest2} ->
            case lists:member(FileName, Acc) of
                false ->
                    findParts(Rest2, [FileName | Acc]);
                true ->
                    findParts(Rest2, Acc)
            end;
        false ->
            findParts(Rest, Acc)
    end.

replaceParts(<<>>, Acc) ->
    Acc;
replaceParts(<<"<img src='", Rest/binary>>, Acc) ->
    case getFileName(Rest) of
        {true, FileName, Rest2} ->
            Cid = integer_to_binary(erlang:phash2(FileName)),
            replaceParts(Rest2, <<Acc/binary, "<img src='cid:", Cid/binary, "'">>);
        false ->
            replaceParts(Rest, <<Acc/binary, "<img src='">>)
    end;
replaceParts(<<"' src='", Rest/binary>>, Acc) ->
    case getFileName(Rest) of
        {true, FileName, Rest2} ->
            Cid = integer_to_binary(erlang:phash2(FileName)),
            replaceParts(Rest2, <<Acc/binary, "' src='cid:", Cid/binary, "'">>);
        false ->
            replaceParts(Rest, <<Acc/binary, "' src='">>)
    end;
replaceParts(<<Char:8, Rest/binary>>, Acc) ->
    replaceParts(Rest, <<Acc/binary, Char:8>>).

getFileName(Rest) ->
    getFileName(Rest, <<>>).

getFileName(<<>>, _Acc) ->
    false;
getFileName(<<"'", Tail/binary>>, Acc) ->
    FileName = binary_to_list(Acc),
    case fileExist(FileName) of
        {true, CompleteFileName} ->
            {true, CompleteFileName, Tail};
        false ->
            false
    end;
getFileName(<<Char:8, Tail/binary>>, Acc) ->
    getFileName(Tail, <<Acc/binary, Char:8>>).

makeMimePart(File) ->
    MimeType = eTodoUtils:mime_type(File),
    [
     ?LF, ?BD, ?LF,
     "Content-Type: ", MimeType, ?LF,
     "Content-Transfer-Encoding: base64", ?LF,
     "Content-Disposition: inline", ?LF,
     "Content-ID: <", integer_to_list(erlang:phash2(File)), ">", ?LF, ?LF,
     makeBody(File), ?LF
    ].

makeBody(FileName) ->
    {ok, MimePart} = file:read_file(FileName),
    encodeBase64(MimePart).

fileExist(FileName) ->
    case filelib:is_file(FileName) of
        true ->
            {true, FileName};
        false ->
            RD = eTodoUtils:getRootDir(),
            CompleteFileName = filename:join([RD, "www"]) ++ FileName,
            case filelib:is_file(CompleteFileName) of
                true ->
                    {true, CompleteFileName};
                false ->
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Encode mime part using base64
%%
%% @end
%%--------------------------------------------------------------------
encodeBase64(MimePart) when is_list(MimePart) ->
    binary_to_list(wrapLongLines(base64:encode(MimePart), <<>>));
encodeBase64(MimePart) when is_binary(MimePart) ->
    wrapLongLines(base64:encode(MimePart), <<>>).

wrapLongLines(<<>>, SoFar) ->
    SoFar;
wrapLongLines(<<Row:76/binary, Rest/binary>>, SoFar) ->
    wrapLongLines(Rest, <<SoFar/binary, Row/binary, 13, 10>>);
wrapLongLines(Rest, SoFar) ->
    <<SoFar/binary, Rest/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make address header
%%
%% @end
%%--------------------------------------------------------------------
makeAddressHeader(Addresses) ->
    makeAddressHeader(Addresses, <<>>).

makeAddressHeader([Address], <<>>) ->
    Address;
makeAddressHeader([Address], Acc) ->
    BinAddr = list_to_binary(Address),
    <<Acc/binary, ",\r\n\t", BinAddr/binary>>;
makeAddressHeader([Address|Rest], <<>>) ->
    BinAddr = list_to_binary(Address),
    makeAddressHeader(Rest, BinAddr);
makeAddressHeader([Address|Rest], Acc) ->
    BinAddr = list_to_binary(Address),
    Acc2    = <<Acc/binary, ",\r\n\t", BinAddr/binary>>,
    makeAddressHeader(Rest, Acc2).
