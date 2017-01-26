%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2017 19:34
%%%-------------------------------------------------------------------
-module(eMd2Html).
-author("mikael.bylund@gmail.com").

%% API
-export([convert/1]).

-define(brTag, "<br />").

-define(punctChar, "!\"#$%&'()*+,--/:;<>=?@[]\\^`{}|~").

%% PS  => Stack built when parsing: [TD]
%% TD  => {Tag, CT, CI}
%% CT  => current token
%% CI  => current indent
%% PL  => previous line
%% CL  => current line
%% Acc => result in progress

convert(MDText) ->
    convert(MDText, [], <<>>, <<>>, <<>>).


convert(MDText, PS, PL, CL, Acc) when is_list(MDText) ->
    convert(unicode:characters_to_binary(MDText), PS, PL, CL, Acc);

%% Support systems with only \n.
convert(<<10, 10, Rest/binary>>, PS, PL, CL, Acc) ->
    convert(<<13, 10, 13, 10, Rest/binary>>, PS, PL, CL, Acc);
convert(<<10, Rest/binary>>, PS, PL, CL, Acc) ->
    convert(<<13, 10, Rest/binary>>, PS, PL, CL, Acc);

%% Default state is paragraph (p)
convert(Content, [], PL, CL, Acc) ->
    convert(Content, [{p, <<>>}], PL, CL, Acc);

%% Backslash escapes
convert(<<"\\\\", Rest/binary>>, PState, PL, CL, Acc) ->
    convert(Rest, addCT(<<"\\">>, PState), PL, CL, Acc);

convert(<<"\\", Char:8, Rest/binary>>, PState, PL, CL, Acc) ->
    case lists:member(Char, ?punctChar) of
        true ->
            Entity = entity(<<Char:8>>),
            convert(Rest, addCT(Entity, PState), PL, CL, Acc);
        false ->
            convert(<<Char:8, Rest/binary>>, addCT(<<"\\">>, PState), PL, CL, Acc)
    end;

%% Block quote
convert(<<$>, Rest/binary>>,
        PState = [{p, CT}|St], PL, CL, Acc) when byte_size(CL) < 4 ->
    case {remWS(CL), lists:member(blockquote, St)} of
        {{<<>>, Count}, false} when Count < 4 ->
            BTag = makeTag(p, CT),
            Acc2 = <<Acc/binary, BTag/binary, "<blockquote>">>,
            convert(Rest, [{p, <<>>}, blockquote|St], PL, CL, Acc2);
        {{<<>>, _}, true} ->
            convert(element(1, remWS(Rest)), [{p, CT}|St], PL, CL, Acc);
        _ ->
            convert(Rest, addCT(<<">">>, PState), PL, CL, Acc)
    end;

%% Close block quote
convert(Content, [blockquote|St], PL, CL, Acc) ->
    Acc2 = <<Acc/binary, "</blockquote>">>,
    convert(Content, St, PL, CL, Acc2);

%% URL
convert(<<$<, Rest/binary>>, PState, PL, CL, Acc) ->
    convert(Rest, [{url, <<>>}|PState], PL, CL, Acc);

convert(<<$>, Rest/binary>>, [{url, CT}|St], PL, CL, Acc) ->
    case catch http_uri:parse(binary_to_list(CT)) of
        {error, _} ->
            convert(<<CT/binary, $>, Rest/binary>>,
                    addCT(<<"<">>, St), PL, CL, Acc);
        _ ->
            Url = <<"<a href='", CT/binary, "'>", CT/binary, "</a>">>,
            convert(Rest, addCT(Url, St), PL, CL, Acc)
    end;

%% Code block
convert(<<13, 10, 32, 32, 32, 32, Rest/binary>>, [{code, CT}], <<>>, <<>>, Acc) ->
    convert(Rest, [{code, <<CT/binary, 13, 10>>}], <<>>, <<>>, Acc);

convert(<<13, 10, 9, Rest/binary>>, [{code, CT}], <<>>, <<>>, Acc) ->
    convert(Rest, [{code, <<CT/binary, 13, 10>>}], <<>>, <<>>, Acc);

convert(<<13, 10, Rest/binary>>, [{code, CT}], <<>>, <<>>, Acc) ->
    BTag = makeTag(code, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, [{p, <<>>}], <<>>, <<>>, Acc2);

convert(<<>>, [{code, CT}], <<>>, <<>>, Acc) ->
    BTag = makeTag(code, CT),
    <<Acc/binary, BTag/binary>>;

convert(<<Char:8, Rest/binary>>, [{code, CT}], <<>>, <<>>, Acc) ->
    convert(Rest, [{code, <<CT/binary, Char:8>>}], <<>>, <<>>, Acc);

convert(<<"    ", Rest/binary>>, [{p, <<>>}], <<>>, <<>>, Acc) ->
    convert(Rest, [{code, <<>>}], <<>>, <<>>, Acc);

convert(<<"    ", Rest/binary>>, [{p, <<>>}], <<>>, <<>>, Acc) ->
    convert(Rest, [{code, <<>>}], <<>>, <<>>, Acc);

convert(<<9, Rest/binary>>, [{p, <<>>}], <<>>, <<>>, Acc) ->
    convert(Rest, [{code, <<>>}], <<>>, <<>>, Acc);

%% Headers
convert(<<"#", Rest/binary>>, PState = [{p, CT}|St], PL, CL, Acc)
  when byte_size(CT) < 4 ->
    case remWS(CT) of
        {<<>>, _} ->
            case headerStart(<<"#", Rest/binary>>) of
                {true, Tag, Rest2} ->
                    BTag = makeTag(p, CT),
                    Acc2 = <<Acc/binary, BTag/binary>>,
                    convert(Rest2, [{Tag, <<>>}|St], PL, CL, Acc2);
                false ->
                    convert(Rest, addCT(<<"#">>, PState), PL, CL, Acc)
            end;
        _ ->
            convert(Rest, addCT(<<"#">>, PState), PL, CL, Acc)
    end;

%% Header end by #...
convert(<<" #", Rest/binary>>, [{Tag, CT}|St], PL, CL, Acc)
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    case headerEnd(Rest) of
        {true, Rest2} ->
            convert(<<Rest2/binary>>, [{Tag, CT}|St], PL, CL, Acc);
        false ->
            convert(Rest, [{Tag, <<CT/binary, " #">>}|St], PL, CL, Acc)
    end;

%% Header end by line end.
convert(<<13, 10, Rest/binary>>, [{Tag, CT}|St], PL, CL, Acc)
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BTag = makeTag(Tag, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, St, PL, CL, Acc2);

%% Paragraph text ending
convert(<<13, 10, 13, 10, Rest/binary>>, [{p, CT}|St], _PL, _CL, Acc) ->
    BTag = makeTag(p, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, St, <<>>, <<>>, Acc2);

%% Paragraph line ending, empty previous line
convert(<<13, 10, Rest/binary>>, [{p, CT}|St], <<>>, CL, Acc) ->
    convert(Rest, [{p, <<CT/binary, 13, 10>>}|St], CL, <<>>, Acc);

convert(<<13, 10, Rest/binary>>, [{p, CT}|St], PL, CL, Acc) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BTag = makeTag(Tag, Hdr),
            Acc2 = <<Acc/binary, BTag/binary, 13, 10>>,
            convert(Rest, St, <<>>, <<>>, Acc2);
        false ->
            case Rest of
                <<13, 10, Rest2/binary>> ->
                    BTag = makeTag(p, CT),
                    Acc2 = <<Acc/binary, BTag/binary>>,
                    convert(Rest2, St, <<>>, <<>>, Acc2);
                Rest ->
                    convert(Rest, [{p, <<CT/binary, 13, 10>>}|St], CL, <<>>, Acc)
            end
    end;

%% Check for list
convert(<<Char:8, Rest/binary>>, [{p, CT}|St], PL, <<>>, Acc) ->
    case checkIfList(<<Char:8, Rest/binary>>) of
        {true, Tag} ->
            BTag = makeTag(p, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(<<Char:8, Rest/binary>>, [Tag|St], <<>>, <<>>, Acc2);
        false ->
            convert(Rest, [{p, <<CT/binary, Char:8>>}|St], PL, <<Char:8>>, Acc)
    end;

%% Parse content
convert(<<Char:8, Rest/binary>>, [{CTag, CT}|St], PL, CL, Acc) ->
    PState = [{CTag, <<CT/binary, Char:8>>}|St],
    convert(Rest, PState, PL, <<CL/binary, Char:8>>, Acc);

%% Text end by end of data
convert(<<>>, [{_Tag, <<>>}], _PL, _CL, Acc) ->
    Acc;

convert(<<>>, [{p, CT}|St], PL, CL, Acc) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            {CTags, _} = closeTags(0, St),
            BTag       = makeTag(Tag, Hdr),
            <<Acc/binary, BTag/binary, CTags/binary>>;
        false ->
            {CTags, _} = closeTags(0, St),
            BTag       = makeTag(p, <<CT/binary>>),
            <<Acc/binary, BTag/binary, CTags/binary>>
    end;

convert(<<>>, PState = [{Tag, _, CT}|_], _PL, _CL, Acc)
    when (Tag == ul) or (Tag == ol) ->
    {CTags, _} = closeTags(0, PState),
    BTag       = makeTag(li, CT),
    <<Acc/binary, BTag/binary, CTags/binary>>;

convert(<<>>, [{url, CT}|St], _PL, _CL, Acc) ->
    {CTags, _} = closeTags(0, St),
    BTag       = makeTag(p, <<$<, CT/binary>>),
    <<Acc/binary, BTag/binary, CTags/binary>>;

convert(<<>>, [{Tag, CT}|St], _PL, _CL, Acc) ->
    {CTags, _} = closeTags(0, St),
    BTag       = makeTag(Tag, CT),
    <<Acc/binary, BTag/binary, CTags/binary>>;

%% Parse Lists
convert(Content, [ol_start|St], PL, CL, Acc) ->
    {true, NumBin, Rest} = parseOLNum(Content),
    Indent = calcIndent(Content, Rest),
    Acc2   = <<Acc/binary, "<ol start='", NumBin/binary, "'>">>,
    convert(Rest, [{ol, Indent, <<>>}|St], PL, CL, Acc2);

convert(Content, [ul_start|St], PL, CL, Acc) ->
    {true, Rest} = parseULBullet(Content),
    Indent = calcIndent(Content, Rest),
    Acc2   = <<Acc/binary, "<ul>">>,
    convert(Rest, [{ul, Indent, <<>>}|St], PL, CL, Acc2);

convert(<<13, 10, Rest/binary>>, PState = [{Tag, Ind, CT}|St], PL, CL, Acc)
  when (Tag == ol) or (Tag == ul) ->
    BTag = makeTag(li, CT),
    case {Tag, parseList(Tag, Rest, Ind)} of
        {_, {true, Rest2}} ->
            Indent2 = calcIndent(Rest, Rest2),
            checkIndent(ul, Indent2, <<>>, Rest2, BTag, PState, PL, CL, Acc);
        {_, {true, Num, Rest2}} ->
            Indent2 = calcIndent(Rest, Rest2),
            checkIndent(ol, Indent2, Num, Rest2, BTag, PState, PL, CL, Acc);
        {ul, false} ->
            convert(Rest, St, PL, CL, <<Acc/binary, BTag/binary, "</ul>">>);
        {ol, false} ->
            convert(Rest, St, PL, CL, <<Acc/binary, BTag/binary, "</ol>">>);
        {Tag, {cont, Rest2}} ->
            convert(Rest2, [{Tag, Ind, <<CT/binary, 13, 10>>}|St], PL, CL, Acc)
    end;
convert(<<Char:8, Rest/binary>>, [{Tag, Ind, CT}|St], PL, CL, Acc)
    when (Tag == ol) or (Tag == ul) ->
    convert(Rest, [{Tag, Ind, <<CT/binary, Char:8>>}|St], PL, CL, Acc).

%%%-------------------------------------------------------------------
%%% Different help functions
%%%-------------------------------------------------------------------

%% Check indentation of ordered and unorded lists.
checkIndent(_, Indent, _, Rest, BTag, [{Tag, Indent, CT}|St], PL, CL, Acc) ->
    BTag = makeTag(li, CT),
    convert(Rest, [{Tag, Indent, <<>>}|St], PL, CL, <<Acc/binary, BTag/binary>>);
checkIndent(Tag, Ind1, Num, Rest, BTag, [{PTag, Ind2, CT}|St], PL, CL, Acc) ->
    case Ind1 > Ind2 of
        true ->
            BTag     = makeTag(li, CT),
            NewState = [{Tag, Ind1, <<>>}, {PTag, Ind2, <<>>}|St],
            StartTag = makeStartTag(Tag, Num),
            Acc2     = <<Acc/binary, BTag/binary, StartTag/binary>>,
            convert(Rest, NewState, PL, CL, Acc2);
        false ->
            {CTags, PState} = closeTags(Ind1, [{PTag, Ind2, CT}|St]),
            Acc2 = <<Acc/binary, BTag/binary, CTags/binary>>,
            convert(Rest, PState, PL, CL, Acc2)
    end.

makeStartTag(ol, Num) ->
    <<"<ol start='", Num/binary, "'>">>;
makeStartTag(ul, _Num) ->
    <<"<ul>">>.

%% Close tags left open or to a specific indentation(used by lists)
closeTags(Ind, PState) ->
    closeTags(Ind, PState, <<>>).

closeTags(_Ind, [], SoFar) ->
    {SoFar, []};
closeTags(Ind, [{ul, Ind, CT}|Rest], SoFar) ->
    {SoFar, [{ul, Ind, CT}|Rest]};
closeTags(Ind1, [{ul, Ind2, _}|Rest], SoFar) when Ind1 < Ind2 ->
    closeTags(Ind1, Rest, <<SoFar/binary, "</ul>">>);
closeTags(Ind, [{ol, Ind, CT}|Rest], SoFar) ->
    {SoFar, [{ol, Ind, CT}|Rest]};
closeTags(Ind1, [{ol, Ind2, _}|Rest], SoFar) when Ind1 < Ind2 ->
    closeTags(Ind1, Rest, <<SoFar/binary, "</ol>">>);
closeTags(Ind1, [blockquote|Rest], SoFar) ->
    closeTags(Ind1, Rest, <<SoFar/binary, "</blockquote>">>);
closeTags(_Ind, PState, SoFar) ->
    {SoFar, PState}.

makeTag(code, Content) ->
    Content2 = entities(Content),
    case remWS(Content2) of
        {<<>>, _} ->
            <<>>;
        {Cont3, _} ->
            <<"<pre><code>", Cont3/binary, "</code></pre>">>
    end;
makeTag(Tag, Content) ->
    BTag     = list_to_binary(atom_to_list(Tag)),
    Content2 = insertBR(Content),
    case remWS(Content2) of
        {<<>>, _} ->
            <<>>;
        {Cont3, _} ->
            <<$<, BTag/binary, $>, Cont3/binary, $<, $/, BTag/binary, $>>>
    end.

headerStart(<<"# ",      Rest/binary>>) -> {true, h1, Rest};
headerStart(<<"## ",     Rest/binary>>) -> {true, h2, Rest};
headerStart(<<"### ",    Rest/binary>>) -> {true, h3, Rest};
headerStart(<<"#### ",   Rest/binary>>) -> {true, h4, Rest};
headerStart(<<"##### ",  Rest/binary>>) -> {true, h5, Rest};
headerStart(<<"###### ", Rest/binary>>) -> {true, h6, Rest};
headerStart(_)                          -> false.

headerEnd(HdrEnd) ->
    headerEnd(HdrEnd, start).

headerEnd(<<>>, _) ->
    {true, <<>>};
headerEnd(<<13, 10, Rest/binary>>, _) ->
    {true, <<13, 10, Rest/binary>>};
headerEnd(<<10, Rest/binary>>, _) ->
    {true, <<13, 10, Rest/binary>>};
headerEnd(<<$#, Rest/binary>>, start) ->
    headerEnd(Rest, start);
headerEnd(<<32, Rest/binary>>, start) ->
    headerEnd(Rest, space);
headerEnd(<<32, Rest/binary>>, space) ->
    headerEnd(Rest, space);
headerEnd(_, space) ->
    false.

header(CT, LL) ->
    case setext(CT) of
        {true, Tag} ->
            checkHeader(Tag, LL);
        false ->
            false
    end.

checkHeader(_  , <<"    ",    _/binary>>) -> false;
checkHeader(Tag, <<"   ",  Rest/binary>>) -> {true, Tag, Rest};
checkHeader(Tag, <<"  ",   Rest/binary>>) -> {true, Tag, Rest};
checkHeader(Tag, <<" ",    Rest/binary>>) -> {true, Tag, Rest};
checkHeader(Tag, Rest)                    -> {true, Tag, Rest}.

setext(Setext) ->
    checkSetext(Setext, start).

checkSetext(<<$=, Rest/binary>>, start) ->
    checkSetext(Rest, h1, underline);
checkSetext(<<$-, Rest/binary>>, start) ->
    checkSetext(Rest, h2, underline);
checkSetext(_CT, _) ->
    false.

checkSetext(<<$=, Rest/binary>>, h1, underline) ->
    checkSetext(Rest, h1, underline);
checkSetext(<<$-, Rest/binary>>, h2, underline) ->
    checkSetext(Rest, h2, underline);
checkSetext(<<>>, Tag, underline) ->
    {true, Tag};
checkSetext(_, _, _) ->
    false.

insertBR(Content) ->
    insertBR(Content, <<>>).

insertBR(<<32, 32, 13, 10, Rest/binary>>, SoFar) ->
    case remWS(Rest) of
        {<<>>, _} ->
            SoFar;
        _ ->
            insertBR(Rest, <<SoFar/binary, ?brTag, 13, 10>>)
    end;
insertBR(Bin = <<Char:8, Rest/binary>>, SoFar) ->
    case remWS(Bin) of
        {<<>>, _} ->
            SoFar;
        _ ->
            insertBR(Rest, <<SoFar/binary, Char:8>>)
    end;
insertBR(<<32, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(<<9, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(<<>>, SoFar) ->
    SoFar.

remWS(Content) ->
    remWS(Content, 0).

remWS(<<>>,                Count) -> {<<>>, Count};
remWS(<<13, Rest/binary>>, Count) -> remWS(Rest, Count + 1);
remWS(<<10, Rest/binary>>, Count) -> remWS(Rest, Count + 1);
remWS(<<32, Rest/binary>>, Count) -> remWS(Rest, Count + 1);
remWS(<<9,  Rest/binary>>, Count) -> remWS(Rest, Count + 4);
remWS(Rest,                Count) -> {Rest, Count}.

checkIfList(Content) ->
    case parseOLNum(Content) of
        {true, _, _} ->
            {true, ol_start};
        false ->
            case parseULBullet(Content) of
                {true, _} ->
                    {true, ul_start};
                false ->
                    false
            end
    end.

parseList(ul, Content, Ind) ->
    case parseULBullet(Content, Ind) of
        false ->
            continueLI(parseOLNum(Content, Ind), Content, Ind);
        Value ->
            Value
    end;
parseList(ol, Content, Ind) ->
    case parseOLNum(Content, Ind) of
        false ->
            continueLI(parseULBullet(Content, Ind), Content, Ind);
        Value ->
            Value
    end.

continueLI(false, Content, Ind) ->
    case remWS(Content) of
        {Content2, Count} when Count >= Ind ->
            {cont, Content2};
        _ ->
            false
    end;
continueLI(Value, _, _) ->
    Value.

parseULBullet(Content) ->
    parseULBullet(Content, 0).

parseULBullet(Content, Ind) ->
    case remWS(Content) of
        {_Content, Count} when Count > (3 + Ind) ->
            false;
        {Content2, _} ->
            doParseULBullet(Content2)
    end.

doParseULBullet(<<"* ", Rest/binary>>) -> {true, element(1, remWS(Rest))};
doParseULBullet(<<"- ", Rest/binary>>) -> {true, element(1, remWS(Rest))};
doParseULBullet(<<"+ ", Rest/binary>>) -> {true, element(1, remWS(Rest))};
doParseULBullet(_Content)              -> false.

parseOLNum(Content) ->
    parseOLNum(Content, 0).

parseOLNum(Content, Ind) ->
    case remWS(Content) of
        {_Content, Count} when Count > (Ind + 3) ->
            false;
        {Content2, _} ->
            doParseOLNum(Content2, <<>>)
    end.

doParseOLNum(<<Num:8, Rest/binary>>, SoFar)
  when ((Num >= $0) and (Num =< $9)) and (byte_size(SoFar) < 10) ->
    doParseOLNum(Rest, <<SoFar/binary, Num:8>>);
doParseOLNum(<<". ", Rest/binary>>, SoFar) ->
    {true, SoFar, element(1, remWS(Rest))};
doParseOLNum(_Content, _SoFar) ->
    false.

addCT(D, [{Tag, CT}|St]) when is_atom(Tag), is_binary(D), is_binary(CT) ->
    [{Tag, <<CT/binary, D/binary>>}|St];
addCT(D, [{Tag, Num, CT}|St]) when is_atom(Tag), is_binary(D), is_binary(CT) ->
    [{Tag, Num, <<CT/binary, D/binary>>}|St].

calcIndent(Bin1, Bin2) -> byte_size(Bin1) - byte_size(Bin2).

entities(Content) ->
    entities(Content, <<>>).

entities(<<>>, Acc) ->
    Acc;
entities(<<Char:8, Rest/binary>>, Acc) ->
    Entity = entity(<<Char:8>>),
    entities(Rest, <<Acc/binary, Entity/binary>>).

entity(<<$<>>) -> <<"&lt;">>;
entity(<<$>>>) -> <<"&gt;">>;
entity(<<$">>) -> <<"&quot;">>;
entity(<<$&>>) -> <<"&amp;">>;
entity(Value)  -> Value.

