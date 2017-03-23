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
    [
        "From: ", To, ?LF,
        "Reply-To: ", ReplyTo, ?LF,
        "Content-Type: multipart/related;", ?LF, 9,
        "boundary=\"", ?BDDef, "\"", ?LF,
        "Subject: ", Subject, ?LF,
        "To: ", To, ?LF,
        "MIME-Version: 1.0", ?LF,
        ?LF,
        ?BD, ?LF,
        "Content-Type: text/html; charset=\"UTF-8\"", ?LF,
        "Content-Transfer-Encoding: quoted-printable", ?LF,
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
    encodeQuotedPrintable(["<!DOCTYPE html><html>",
                           [headTag(
                              [titleTag(["eTodo - ", UserName]),
                               metaTag([{"http-equiv", "Content-Type"},
                                        {content, "text/html; charset=UTF-8"}]),
                               styleTag([{type, "text/css"}], Styles)
                              ]),
                            bodyTag([Messages2])]]).

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
%% Encode mime part using base64
%%
%% @end
%%--------------------------------------------------------------------
encodeQuotedPrintable(MimePart) when is_list(MimePart) ->
    binary_to_list(encodeQP(iolist_to_binary(MimePart), <<>>, <<>>));
encodeQuotedPrintable(MimePart) when is_binary(MimePart) ->
    encodeQP(MimePart, <<>>, <<>>).

encodeQP(<<>>, _LastLine, SoFar) ->
    SoFar;
%% CRLF clear last line and continue.
encodeQP(<<13, 10, Rest/binary>>, _LastLine, SoFar) ->
    encodeQP(Rest, <<>>, <<SoFar/binary, 13, 10>>);
%% Keep space and tab if it isn't at the end of a line.
encodeQP(<<WS, 13, 10, Rest/binary>>, LastLine, SoFar)
  when ((WS == 32) or (WS == 9)) and (size(LastLine) < 73) ->
    Encoded = encode(WS),
    encodeQP(Rest, <<>>, <<SoFar/binary, Encoded/binary>>);
%% Insert soft line break if needed when encoding space and tab.
encodeQP(<<WS, 13, 10, Rest/binary>>, _LastLine, SoFar)
  when ((WS == 32) or (WS == 9)) ->
    Encoded = encode(WS),
    SoFar2 = <<SoFar/binary, $=, 13, 10, Encoded/binary>>,
    encodeQP(Rest, <<>>, SoFar2);
%% No need to insert soft line break or encode character.
encodeQP(<<Char:8, Rest/binary>>, LastLine, SoFar)
  when ((Char == 32) or (Char == 9)
        or ((Char > 32) and (Char < 61)) or ((Char > 61) and (Char < 127))) and
       (size(LastLine) < 75) ->
    LastLine2 = <<LastLine/binary, Char:8>>,
    encodeQP(Rest, LastLine2, <<SoFar/binary, Char:8>>);
%% Just one character before line break, no need to use soft line break.
encodeQP(<<Char:8, 13, 10, Rest/binary>>, _LastLine, SoFar)
  when ((Char == 32) or (Char == 9)
        or ((Char > 32) and (Char < 61)) or ((Char > 61) and (Char < 127))) ->
    SoFar2 = <<SoFar/binary, Char:8, 13, 10>>,
    encodeQP(Rest, <<>>, SoFar2);
%% Insert soft line break, character doesn't need to be encoded.
encodeQP(<<Char:8, Rest/binary>>, _LastLine, SoFar)
  when ((Char == 32) or (Char == 9)
        or ((Char > 32) and (Char < 61)) or ((Char > 61) and (Char < 127))) ->
    SoFar2 = <<SoFar/binary, $=, 13, 10, Char:8>>,
    encodeQP(Rest, <<Char:8>>, SoFar2);
%% Encode character, no need for soft line break.
encodeQP(<<Char:8, Rest/binary>>, LastLine, SoFar)
  when size(LastLine) < 73 ->
    Encoded = encode(Char),
    LastLine2 = <<LastLine/binary, Encoded/binary>>,
    SoFar2 = <<SoFar/binary, Encoded/binary>>,
    encodeQP(Rest, LastLine2, SoFar2);
%% Encode character and insert soft line break.
encodeQP(<<Char:8, Rest/binary>>, _LastLine, SoFar) ->
    Encoded = encode(Char),
    SoFar2 = <<SoFar/binary, $=, 13, 10, Encoded/binary>>,
    encodeQP(Rest, Encoded, SoFar2).

encode(Char) when Char < 16 ->
    [[_A, B]] = io_lib:format("~2.16.0B", [Char]),
    <<$=, 48, B>>;
encode(Char) ->
    [[A, B]] = io_lib:format("~2.16.0B", [Char]),
    <<$=, A, B>>.

