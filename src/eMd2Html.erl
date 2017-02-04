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
-export([convert/1, dbg/0, dbg/1, debug/1, remInd/1]).

-define(brTag, "<br />").

-define(punct, "!\"#$%&'()*+,--/:;<>=?@[]\\^`{}|~").

%% PS  => Stack built when parsing: [TD]
%% TD  => {Tag, CT, CI}
%% CT  => current token
%% CI  => current indent
%% PL  => previous line
%% CL  => current line
%% Acc => result in progress

convert(MDText) when is_list(MDText) ->
    convert(unicode:characters_to_binary(MDText));
convert(MDText) ->
    convert(<<13, 10, MDText/binary>>, [{p, <<>>}], <<>>, <<>>, <<>>).

%% Handle no state as paragraph (p)
convert(Content, [], PL, CL, Acc) ->
    convert(Content, [{p, <<>>}], PL, CL, Acc);

convert(<<13, 10, Content/binary>>, PState, _PL, CL, Acc) ->
    Content2 = evalRemWS(Content, PState),
    parse(<<13, 10, Content2/binary>>, PState, CL, getCurrentRow(Content), Acc);

convert(<<10, Content/binary>>, PState, _PL, CL, Acc) ->
    Content2 = evalRemWS(Content, PState),
    parse(<<13, 10, Content2/binary>>, PState, CL, getCurrentRow(Content), Acc);

convert(Content, PState, PL, CL, Acc) ->
    parse(Content, PState, PL, CL, Acc).


%% Backslash escapes
parse(<<"\\\\", Rest/binary>>, PState, PL, CL, Acc) ->
    convert(Rest, addCT(<<"\\">>, PState), PL, CL, Acc);

parse(<<"\\", Char:8, Rest/binary>>, PState, PL, CL, Acc) ->
    case lists:member(Char, ?punct) of
        true ->
            Entity = entity(<<Char:8>>),
            convert(Rest, addCT(Entity, PState), PL, CL, Acc);
        false ->
            convert(<<Char:8, Rest/binary>>, addCT(<<"\\">>, PState), PL, CL, Acc)
    end;

%% Block quote
parse(<<13, 10, $>, Rest/binary>>, PState = [{p, CT}|St], PL, CL, Acc) ->
    case lists:member(blockquote, St) of
        false ->
            BTag = makeTag(p, CT),
            Acc2 = <<Acc/binary, BTag/binary, "<blockquote>">>,
            convert(Rest, [{p, <<>>}, blockquote|St], PL, CL, Acc2);
        true ->
            BlockQuoteText = element(1, remWS(Rest)),
            convert(<<13, 10, BlockQuoteText/binary>>,
                    [{p, <<CT/binary>>}|St], PL, CL, Acc);
        _ ->
            convert(Rest, addCT(<<13, 10, $>>>, PState), PL, CL, Acc)
    end;

%% Close block quote
parse(Content, [blockquote| St], PL, CL, Acc) ->
    Acc2 = <<Acc/binary, "</blockquote>">>,
    convert(Content, St, PL, CL, Acc2);

%% URL
parse(<<$<, Rest/binary>>, PState, PL, CL, Acc) ->
    convert(Rest, [{url, <<>>}|PState], PL, CL, Acc);

parse(<<$>, Rest/binary>>, [{url, CT}| St], PL, CL, Acc) ->
    case catch http_uri:parse(binary_to_list(CT)) of
        {error, _} ->
            convert(<<CT/binary, $>, Rest/binary>>,
                    addCT(<<"<">>, St), PL, CL, Acc);
        _ ->
            Url = <<"<a href='", CT/binary, "'>", CT/binary, "</a>">>,
            convert(Rest, addCT(Url, St), PL, CL, Acc)
    end;

%% Indented code block
parse(<<13, 10, Rest/binary>>, [{code, CT}], PL, CL, Acc) ->
    case remInd(Rest) of
        {Rest2, Count} when Count >= 4, is_binary(Rest2) ->
            convert(Rest2, [{code, <<CT/binary, 13, 10>>}], PL, CL, Acc);
        _ ->
            BTag = makeTag(code, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(Rest, [{p, <<>>}], PL, CL, Acc2)
    end;

parse(<<>>, [{Tag, CT}], _PL, _CL, Acc)
  when (Tag == code) or (Tag == f1_code) or (Tag == f2_code) ->
    BTag = makeTag(code, CT),
    <<Acc/binary, BTag/binary>>;

%% Paragraph text ending
parse(<<13, 10, Rest/binary>>, [{p, CT}| St], PL, <<>>, Acc) ->
    BTag = makeTag(p, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, St, PL, <<>>, Acc2);

parse(<<13, 10, Rest/binary>>, [{p, CT}| St], PL, CL, Acc) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BTag = makeTag(Tag, Hdr),
            Size = byte_size(CL),
            Acc2 = <<Acc/binary, BTag/binary>>,
            <<CL:Size/bytes, Rest2/binary>> = Rest,
            convert(Rest2, St, PL, CL, Acc2);
        false ->
            CodeIndent = getCodeIndent(St),
            case remWS(Rest) of
                {Rest2, Count} when Count >= CodeIndent ->
                    convert(Rest2, [{code, <<>>}], PL, CL, Acc);
                _ ->
                    convert(Rest, [{p, <<CT/binary, 13, 10>>}|St], PL, CL, Acc)
            end
    end;

%% Start fenced code block
parse(<<"~~~", Rest/binary>>, [{p, _CT}], PL, <<"~~~">>, Acc) ->

    convert(Rest, [{f1_code, <<>>}], PL, <<"~~~">>, Acc);
parse(<<"```", Rest/binary>>, [{p, _CT}], PL, <<"```">>, Acc) ->
    convert(Rest, [{f2_code, <<>>}], PL, <<"```">>, Acc);

%% Stop fenced code block
parse(<<"~~~", Rest/binary>>, [{f1_code, CT}], PL, <<"~~~">>, Acc) ->
    BTag = makeTag(code, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, [{p, <<>>}], PL, <<"~~~">>, Acc2);
parse(<<"```", Rest/binary>>, [{f2_code, CT}], PL, <<"```">>, Acc) ->
    BTag = makeTag(code, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, [{p, <<>>}], PL, <<"~~~">>, Acc2);

%% Parse code block
parse(<<13, 10, Rest/binary>>, [{Tag, CT}], PL, CL, Acc)
  when (Tag == code) or (Tag == f1_code) or (Tag == f2_code) ->
    convert(Rest, [{Tag, <<CT/binary, 13, 10>>}], PL, CL, Acc);
parse(<<Char:8, Rest/binary>>, [{Tag, CT}], PL, CL, Acc)
  when (Tag == code) or (Tag == f1_code) or (Tag == f2_code) ->
    convert(Rest, [{Tag, <<CT/binary, Char:8>>}], PL, CL, Acc);

%% Headers
parse(<<"#", Rest/binary>>, PState = [{p, CT}| St], PL, CL, Acc) ->
    case headerStart(<<"#", Rest/binary>>) of
        {true, Tag, Rest2} ->
            BTag = makeTag(p, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(Rest2, [{Tag, <<>>}|St], PL, CL, Acc2);
        false ->
            convert(Rest, addCT(<<"#">>, PState), PL, CL, Acc)
    end;

%% Header end by #...
parse(<<" #", Rest/binary>>, [{Tag, CT}| St], PL, CL, Acc)
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    case headerEnd(Rest) of
        {true, Rest2} ->
            convert(<<Rest2/binary>>, [{Tag, CT}|St], PL, CL, Acc);
        false ->
            convert(Rest, [{Tag, <<CT/binary, " #">>}|St], PL, CL, Acc)
    end;

%% Header end by line end.
parse(<<13, 10, Rest/binary>>, [{Tag, CT}| St], PL, CL, Acc)
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BTag = makeTag(Tag, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, St, PL, CL, Acc2);

%% Check for list
parse(<<Char:8, Rest/binary>>, [{p, CT}| St], PL, CL, Acc) ->
    case checkIfList(<<Char:8, Rest/binary>>) of
        {true, Tag} ->
            BTag = makeTag(p, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(<<Char:8, Rest/binary>>, [Tag|St], PL, CL, Acc2);
        false ->
            convert(Rest, [{p, <<CT/binary, Char:8>>}|St], PL, CL, Acc)
    end;

%% Parse content
parse(<<Char:8, Rest/binary>>, [{CTag, CT}| St], PL, CL, Acc) ->
    PState = [{CTag, <<CT/binary, Char:8>>}|St],
    convert(Rest, PState, PL, CL, Acc);

%% Text end by end of data
parse(<<>>, [{_Tag, <<>>}], _PL, _CL, Acc) ->
    Acc;

parse(<<>>, [{p, CT}| St], PL, CL, Acc) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            {CTags, _} = closeTags(St),
            BTag       = makeTag(Tag, Hdr),
            <<Acc/binary, BTag/binary, CTags/binary>>;
        false ->
            {CTags, _} = closeTags(St),
            BTag       = makeTag(p, <<CT/binary>>),
            <<Acc/binary, BTag/binary, CTags/binary>>
    end;

parse(<<>>, PState = [{Tag, _, CT}| _], _PL, _CL, Acc)
  when (Tag == ul) or (Tag == ol) ->
    {CTags, _} = closeTags(PState),
    BTag       = makeTag(li, CT),
    <<Acc/binary, BTag/binary, CTags/binary>>;

parse(<<>>, [{url, CT}| St], _PL, _CL, Acc) ->
    {CTags, _} = closeTags(St),
    BTag       = makeTag(p, <<$<, CT/binary>>),
    <<Acc/binary, BTag/binary, CTags/binary>>;

parse(<<>>, [{Tag, CT}| St], _PL, _CL, Acc) ->
    {CTags, _} = closeTags(St),
    BTag       = makeTag(Tag, CT),
    <<Acc/binary, BTag/binary, CTags/binary>>;

%% Parse Lists
parse(Content, [ol_start| St], PL, CL, Acc) ->
    {true, NumBin, Rest} = parseOLNum(Content),
    Indent = calcIndent(CL, Content, Rest),
    Acc2   = <<Acc/binary, "<ol start='", NumBin/binary, "'>">>,
    convert(Rest, [{ol, Indent, <<>>}|St], PL, CL, Acc2);

parse(Content, [ul_start| St], PL, CL, Acc) ->
    {true, Rest} = parseULBullet(Content),
    Indent = calcIndent(CL, Content, Rest),
    Acc2   = <<Acc/binary, "<ul>">>,
    convert(Rest, [{ul, Indent, <<>>}|St], PL, CL, Acc2);

parse(<<13, 10, Rest/binary>>, PState = [{Tag, Ind, CT}| St], PL, CL, Acc)
  when (Tag == ol) or (Tag == ul) ->
    BTag = makeTag(li, CT),
    case {Tag, parseList(Tag, Rest, Ind)} of
        {_, {true, Rest2}} ->
            Indent2 = calcIndent(CL, Rest, Rest2),
            checkIndent(ul, Indent2, <<>>, Rest2, BTag, PState, PL, CL, Acc);
        {_, {true, Num, Rest2}} ->
            Indent2 = calcIndent(CL, Rest, Rest2),
            checkIndent(ol, Indent2, Num, Rest2, BTag, PState, PL, CL, Acc);
        {ul, false} ->
            convert(Rest, St, PL, CL, <<Acc/binary, BTag/binary, "</ul>">>);
        {ol, false} ->
            convert(Rest, St, PL, CL, <<Acc/binary, BTag/binary, "</ol>">>);
        {Tag, {cont, Rest2}} ->
            convert(Rest2, [{Tag, Ind, <<CT/binary, 13, 10>>}|St], PL, CL, Acc)
    end;
parse(<<Char:8, Rest/binary>>, [{Tag, Ind, CT}| St], PL, CL, Acc)
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


closeTags(PState) ->
    closeTags(-1, PState, <<>>).

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

header(CT, PL) ->
    case setext(CT) of
        {true, Tag} ->
            case remLS(PL) of
                {Rest, Count} when Count =< 3 ->
                    {true, Tag, Rest};
                _->
                    false
            end;
        false ->
            false
    end.

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
    evalInsertBR(Rest, SoFar);
insertBR(<<"\\", 13, 10, Rest/binary>>, SoFar) ->
    evalInsertBR(Rest, SoFar);
insertBR(<<32, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(<<9, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(Bin = <<Char:8, Rest/binary>>, SoFar) ->
    case remWS(Bin) of
        {<<>>, _} ->
            SoFar;
        _ ->
            insertBR(Rest, <<SoFar/binary, Char:8>>)
    end;
insertBR(<<>>, SoFar) ->
    SoFar.

evalInsertBR(Rest, SoFar) ->
    case remWS(Rest) of
        {<<>>, _} ->
            SoFar;
        {Rest2, _} ->
            insertBR(Rest2, <<SoFar/binary, ?brTag, 13, 10>>)
    end.

remWS(Content) ->
    remWS(Content, infinity, 0, all).

remLS(Content) ->
    remWS(Content, infinity, 0, true).

remInd(Content) ->
    remWS(Content, 4, 0, true).

remWS(<<>>,                    _, C, _)      -> {<<>>, C};
remWS(Rest, Ind, Count, _) when Count >= Ind -> {Rest, Count};
remWS(<<13, 10, Rest/binary>>, I, C, false)  -> remWS(Rest, I, C,     true);
remWS(<<13, 10, Rest/binary>>, I, C, all)    -> remWS(Rest, I, C,     all);
remWS(<<32,     Rest/binary>>, I, C, LRem)   -> remWS(Rest, I, C + 1, LRem);
remWS(<<9,      Rest/binary>>, I, C, LRem)   -> remWS(Rest, I, C + 4, LRem);
remWS(Rest,                    _, C, _)      -> {Rest, C}.

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
    case remLS(Content) of
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
    case remLS(Content) of
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
    case remLS(Content) of
        {_Content, Count} when Count > (Ind + 3) ->
            false;
        {Content2, _} ->
            doParseOLNum(Content2, <<>>, start)
    end.

doParseOLNum(<<Num:8, Rest/binary>>, SoFar, State)
  when ((Num >= $0) and (Num =< $9)) and (byte_size(SoFar) < 10) and
       (((State == start) or (State == cont))) ->
    doParseOLNum(Rest, <<SoFar/binary, Num:8>>, cont);
doParseOLNum(<<". ", Rest/binary>>, SoFar, cont) ->
    {true, SoFar, element(1, remWS(Rest))};
doParseOLNum(_Content, _SoFar, _) ->
    false.

addCT(D, [{Tag, CT}|St]) when is_atom(Tag), is_binary(D), is_binary(CT) ->
    [{Tag, <<CT/binary, D/binary>>}|St];
addCT(D, [{Tag, Num, CT}|St]) when is_atom(Tag), is_binary(D), is_binary(CT) ->
    [{Tag, Num, <<CT/binary, D/binary>>}|St].

calcIndent(CurrentLine, Content, Rest) ->
    {_, Count} = remWS(CurrentLine),
    Count + byte_size(Content) - byte_size(Rest).

entities(Content) ->
    entities(Content, <<>>).

entities(<<>>, Acc) ->
    Acc;
entities(<<Char:8, Rest/binary>>, Acc) ->
    Entity = entity(<<Char:8>>),
    entities(Rest, <<Acc/binary, Entity/binary>>).

entity(<<$<>>)  -> <<"&lt;">>;
entity(<<$>>>)  -> <<"&gt;">>;
entity(<<$">>)  -> <<"&quot;">>;
entity(<<$&>>)  -> <<"&amp;">>;
entity(<<160>>) -> <<"§nbsp;">>;
entity(Value)   -> Value.

getCodeIndent([{_Tag, Ind, _CT}|_]) -> Ind + 4;
getCodeIndent(_)                    -> 4.

%% Get current row
getCurrentRow(Content) ->
    getCurrentRow(Content, <<>>).

getCurrentRow(<<>>,                     Acc) -> Acc;
getCurrentRow(<<13, 10, _Rest/binary>>, Acc) -> Acc;
getCurrentRow(<<10,     _Rest/binary>>, Acc) -> Acc;
getCurrentRow(<<Char:8,  Rest/binary>>, Acc) ->
    getCurrentRow(Rest, <<Acc/binary, Char:8>>).

evalRemWS(Content, [{Tag, _, _}|_]) when (Tag == ul) or (Tag == ol) ->
    Content;
evalRemWS(Content, _PState) ->
    case remLS(Content) of
        {Rest2, Count} when Count =< 3 ->
            Rest2;
        _ ->
            Content
    end.

%%%-------------------------------------------------------------------
%%% Debug info
%%%-------------------------------------------------------------------

dbg() ->
    redbug:stop(),
    timer:sleep(500),
    Dbg = ["eMd2Html:convert", "eMd2Html:parse"],
    redbug:start(Dbg, [{msgs, 1000}, {time, 100000}]).

dbg(Info) ->
    redbug:stop(),
    timer:sleep(500),
    Info2 = ["eMd2Html:" ++ Value || Value <- Info],
    redbug:start(Info2, [{msgs, 1000}, {time, 100000}]).

debug(Info) ->
    redbug:stop(),
    timer:sleep(500),
    redbug:start(Info, [{msgs, 1000}, {time, 100000}]).

