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
-export([convert/1, convert/2, remInd/1]).

-define(brTag, "<br />").

-define(punct, "!\"#$%&'()*+,--/:;<>=?@[]\\^`{}|~").

%% PS  => Stack built when parsing: [TD]
%% TD  => {Tag, CT, CI}
%% CT  => current token
%% PT  => previous token
%% CI  => current indent
%% PL  => previous line
%% CL  => current line
%% Acc => result in progress

convert(MDText) when is_list(MDText) ->
    convert(eGuiFunctions:filterAndConvert2Utf8(MDText));
convert(MDText) when is_binary(MDText) ->
    Dir     = list_to_binary(eTodoUtils:getRootDir()),
    MDText2 = convertLineEnding(MDText, crlf2lf),
    convert(<<10, MDText2/binary>>, [{p, <<>>}], <<>>, <<>>, Dir, <<>>).

convert(MDText, Dir) when is_list(MDText), is_list(Dir) ->
    convert(eGuiFunctions:filterAndConvert2Utf8(MDText), list_to_binary(Dir));
convert(MDText, Dir) when is_binary(MDText), is_binary(Dir) ->
    convert(<<10, MDText/binary>>, [{p, <<>>}], <<>>, <<>>, Dir, <<>>).

%% Handle no state as paragraph (p)
convert(Content, [], PL, CL, Dir, Acc) ->
    convert(Content, [{p, <<>>}], PL, CL, Dir, Acc);

convert(<<10, Content/binary>>, PState, _PL, CL, Dir, Acc) ->
    Content2 = evalRemWS(Content, PState),
    parse(<<10, Content2/binary>>, PState, CL, getCurrentRow(Content), Dir, Acc);

convert(Content, PState, PL, CL, Dir, Acc) ->
    parse(Content, PState, PL, CL, Dir, Acc).

%% Block quote
parse(<<10, $>, Rest/binary>>, PState = [{p, CT}|St], PL, CL, Dir, Acc) ->
    case lists:member(blockquote, St) of
        false ->
            BTag = makeTag(p, Dir, CT),
            Acc2 = <<Acc/binary, BTag/binary, "<blockquote>">>,
            convert(Rest, [{p, <<>>}, blockquote|St], PL, CL, Dir, Acc2);
        true ->
            BlockQuoteText = element(1, remWS(Rest)),
            convert(<<10, BlockQuoteText/binary>>,
                    [{p, <<CT/binary>>}|St], PL, CL, Dir, Acc);
        _ ->
            convert(Rest, addCT(<<10, $>>>, PState), PL, CL, Dir, Acc)
    end;

%% Close block quote
parse(Content, [blockquote| St], PL, CL, Dir, Acc) ->
    Acc2 = <<Acc/binary, "</blockquote>">>,
    convert(Content, St, PL, CL, Dir, Acc2);

%% Indented code block
parse(<<10, Rest/binary>>, [{code, CT}], PL, CL, Dir, Acc) ->
    case remInd(Rest) of
        {Rest2, Count} when Count >= 4, is_binary(Rest2) ->
            convert(Rest2, [{code, <<CT/binary, 10>>}], PL, CL, Dir, Acc);
        _ ->
            BTag = makeTag(code, Dir, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(Rest, [{p, <<>>}], PL, CL, Dir, Acc2)
    end;

parse(<<>>, [{Tag, CT}], _PL, _CL, Dir, Acc)
  when (Tag == code) or (Tag == f1_code) or (Tag == f2_code) ->
    BTag = makeTag(code, Dir, CT),
    <<Acc/binary, BTag/binary>>;

%% Start fenced code block
parse(<<"~~~", Rest/binary>>, [{p, _CT}], PL, <<"~~~">>, Dir, Acc) ->

    convert(Rest, [{f1_code, <<>>}], PL, <<"~~~">>, Dir, Acc);
parse(<<"```", Rest/binary>>, [{p, _CT}], PL, <<"```">>, Dir, Acc) ->
    convert(Rest, [{f2_code, <<>>}], PL, <<"```">>, Dir, Acc);

%% Stop fenced code block
parse(<<"~~~", Rest/binary>>, [{f1_code, CT}], PL, <<"~~~">>, Dir, Acc) ->
    BTag = makeTag(code, Dir, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, [{p, <<>>}], PL, <<"~~~">>, Dir, Acc2);
parse(<<"```", Rest/binary>>, [{f2_code, CT}], PL, <<"```">>, Dir, Acc) ->
    BTag = makeTag(code, Dir, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, [{p, <<>>}], PL, <<"~~~">>, Dir, Acc2);

%% Parse code block
parse(<<10, Rest/binary>>, [{Tag, CT}], PL, CL, Dir, Acc)
    when (Tag == code) or (Tag == f1_code) or (Tag == f2_code) ->
    convert(Rest, [{Tag, <<CT/binary, 10>>}], PL, CL, Dir, Acc);
parse(<<Char:8, Rest/binary>>, [{Tag, CT}], PL, CL, Dir, Acc)
    when (Tag == code) or (Tag == f1_code) or (Tag == f2_code) ->
    convert(Rest, [{Tag, <<CT/binary, Char:8>>}], PL, CL, Dir, Acc);

%% Backslash escapes
parse(<<"\\\\", Rest/binary>>, PState, PL, CL, Dir, Acc) ->
    convert(Rest, addCT(<<"\\">>, PState), PL, CL, Dir, Acc);

parse(<<"\\", Char:8, Rest/binary>>, PState, PL, CL, Dir, Acc) ->
    case lists:member(Char, ?punct) of
        true ->
            Entity = entity(<<Char:8>>),
            convert(Rest, addCT(Entity, PState), PL, CL, Dir, Acc);
        false ->
            convert(<<Char:8, Rest/binary>>, addCT(<<"\\">>, PState), PL, CL, Dir, Acc)
    end;

%% Paragraph text ending
parse(<<10, Rest/binary>>, [{p, CT}| St], PL, <<>>, Dir, Acc) ->
    BTag = makeTag(p, Dir, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, St, PL, <<>>, Dir, Acc2);

parse(<<10, Rest/binary>>, [{p, CT}| St], PL, CL, Dir, Acc) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BTag = makeTag(Tag, Dir, Hdr),
            Size = byte_size(CL),
            Acc2 = <<Acc/binary, BTag/binary>>,
            <<CL:Size/bytes, Rest2/binary>> = Rest,
            convert(Rest2, St, PL, CL, Dir, Acc2);
        false ->
            CodeIndent = getCodeIndent(St),
            case remWS(Rest) of
                {Rest2, Count} when Count >= CodeIndent ->
                    convert(Rest2, [{code, <<>>}], PL, CL, Dir, Acc);
                _ ->
                    convert(Rest, [{p, <<CT/binary, 10>>}|St], PL, CL, Dir, Acc)
            end
    end;

%% URL
parse(<<$<, Rest/binary>>, PState, PL, CL, Dir, Acc) ->
    convert(Rest, [{url, <<>>}|PState], PL, CL, Dir, Acc);

parse(<<$>, Rest/binary>>, [{url, CT}| St], PL, CL, Dir, Acc) ->
    case catch uri_string:parse(binary_to_list(CT)) of
        #{host := Host, scheme := "https"} when Host =/= "" ->
            Url = <<"<a href='", CT/binary, "'>", CT/binary, "</a>">>,
            convert(Rest, addCT(Url, St), PL, CL, Dir, Acc);
        _ ->
            %% Convert to &lt to remove html support from markdown
            convert(<<CT/binary, $>, Rest/binary>>,
                    addCT(<<"&lt;">>, St), PL, CL, Dir, Acc)
    end;

%% ImgLink
parse(<<$[, Rest/binary>>, [{ilDesc, Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(Rest, [{ilDesc, Num + 1, <<CT/binary, $[>>}|PState], PL, CL, Dir, Acc);

parse(<<$!, $[, Rest/binary>>, PState, PL, CL, Dir, Acc) ->
    convert(Rest, [{ilDesc, 1, <<>>}|PState], PL, CL, Dir, Acc);

parse(<<$], $(, Rest/binary>>, [{ilDesc, 1, CT}| St], PL, CL, Dir, Acc) ->
    convert(Rest, [{ilink, 1, <<>>}, {ilDesc, CT}|St], PL, CL, Dir, Acc);

parse(<<$], Rest/binary>>, [{ilDesc, Num, CT}|PState], PL, CL, Dir, Acc)
    when Num > 1 ->
    convert(Rest, [{ilDesc, Num - 1, <<CT/binary, $]>>}|PState], PL, CL, Dir, Acc);

parse(<<$], Rest/binary>>, [{ilDesc, _Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(<<CT/binary, $], Rest/binary>>,
            addCT(<<"![">>, PState), PL, CL, Dir, Acc);

parse(<<Char:8, Rest/binary>>, [{ilDesc, Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(Rest, [{ilDesc, Num, <<CT/binary, Char:8>>}|PState], PL, CL, Dir, Acc);

parse(<<$(, Rest/binary>>, [{ilink, 1, CT}|St], PL, CL, Dir, Acc) ->
    convert(Rest, [{ilink, 2, <<CT/binary, $(>>}|St], PL, CL, Dir, Acc);

parse(<<$), Rest/binary>>, [{ilink, 2, CT}|St], PL, CL, Dir, Acc) ->
    convert(Rest, [{ilink, 1, <<CT/binary, $)>>}|St], PL, CL, Dir, Acc);

parse(<<$), Rest/binary>>, [{ilink, 1, CT}, {ilDesc, PT}|St], PL, CL, Dir, Acc) ->
    case catch uri_string:parse(binary_to_list(removeParam(CT))) of
        #{host := Host, scheme := "https"} when Host =/= "" ->
            case eGuiFunctions:convertToLocal(CT) of
                {Original, LUrl} ->
                    Url  = <<"<img src='", LUrl/binary, "' alt='", PT/binary, "' />">>,
                    HRef = <<"<a href='", Original/binary, "'>", Url/binary, "</a>">>,
                    convert(Rest, addCT(HRef, St), PL, CL, Dir, Acc);
                CT ->
                    Url  = <<"<img src='", CT/binary, "' alt='", PT/binary, "' />">>,
                    convert(Rest, addCT(Url, St), PL, CL, Dir, Acc)
            end;
        _ ->
            convert(<<PT/binary, $], $(, CT/binary, $), Rest/binary>>,
                addCT(<<"![">>, St), PL, CL, Dir, Acc)
    end;

parse(<<Char:8, Rest/binary>>, [{ilink, Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(Rest, [{ilink, Num, <<CT/binary, Char:8>>}|PState], PL, CL, Dir, Acc);

%% Link
parse(<<$[, Rest/binary>>, [{lDesc, Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(Rest, [{lDesc, Num + 1, <<CT/binary, $[>>}|PState], PL, CL, Dir, Acc);

parse(<<$[, Rest/binary>>, PState, PL, CL, Dir, Acc) ->
    convert(Rest, [{lDesc, 1, <<>>}|PState], PL, CL, Dir, Acc);

parse(<<$], $(, Rest/binary>>, [{lDesc, 1, CT}| St], PL, CL, Dir, Acc) ->
    convert(Rest, [{link, 1, <<>>}, {lDesc, CT}|St], PL, CL, Dir, Acc);

parse(<<$], Rest/binary>>, [{lDesc, Num, CT}|PState], PL, CL, Dir, Acc)
    when Num > 1 ->
    convert(Rest, [{lDesc, Num - 1, <<CT/binary, $]>>}|PState], PL, CL, Dir, Acc);

parse(<<$], Rest/binary>>, [{lDesc, _Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(<<CT/binary, $], Rest/binary>>,
            addCT(<<"[">>, PState), PL, CL, Dir, Acc);

parse(<<Char:8, Rest/binary>>, [{lDesc, Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(Rest, [{lDesc, Num, <<CT/binary, Char:8>>}|PState], PL, CL, Dir, Acc);

parse(<<$(, Rest/binary>>, [{link, 1, CT}|St], PL, CL, Dir, Acc) ->
    convert(Rest, [{link, 2, <<CT/binary, $(>>}|St], PL, CL, Dir, Acc);

parse(<<$), Rest/binary>>, [{link, 2, CT}|St], PL, CL, Dir, Acc) ->
    convert(Rest, [{link, 1, <<CT/binary, $)>>}|St], PL, CL, Dir, Acc);

parse(<<$), Rest/binary>>, [{link, 1, CT}, {lDesc, PT}|St], PL, CL, Dir, Acc) ->
    case catch uri_string:parse(binary_to_list(removeParam(CT))) of
        #{host := Host, scheme := "https"} when Host =/= "" ->
            Url = <<"<a href='", CT/binary, "'>", PT/binary, "</a>">>,
            convert(Rest, addCT(Url, St), PL, CL, Dir, Acc);
        _ ->
            convert(<<PT/binary, $], $(, CT/binary, $), Rest/binary>>,
                addCT(<<"[">>, St), PL, CL, Dir, Acc)
    end;

parse(<<Char:8, Rest/binary>>, [{link, Num, CT}|PState], PL, CL, Dir, Acc) ->
    convert(Rest, [{link, Num, <<CT/binary, Char:8>>}|PState], PL, CL, Dir, Acc);

%% Headers
parse(<<"#", Rest/binary>>, PState = [{p, CT}| St], PL, CL, Dir, Acc) ->
    case headerStart(<<"#", Rest/binary>>) of
        {true, Tag, Rest2} ->
            BTag = makeTag(p, Dir, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(Rest2, [{Tag, <<>>}|St], PL, CL, Dir, Acc2);
        false ->
            convert(Rest, addCT(<<"#">>, PState), PL, CL, Dir, Acc)
    end;

%% Header end by #...
parse(<<" #", Rest/binary>>, [{Tag, CT}| St], PL, CL, Dir, Acc)
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    case headerEnd(Rest) of
        {true, Rest2} ->
            convert(<<Rest2/binary>>, [{Tag, CT}|St], PL, CL, Dir, Acc);
        false ->
            convert(Rest, [{Tag, <<CT/binary, " #">>}|St], PL, CL, Dir, Acc)
    end;

%% Header end by line end.
parse(<<10, Rest/binary>>, [{Tag, CT}| St], PL, CL, Dir, Acc)
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BTag = makeTag(Tag, Dir, CT),
    Acc2 = <<Acc/binary, BTag/binary>>,
    convert(Rest, St, PL, CL, Dir, Acc2);

%% Check for list
parse(<<Char:8, Rest/binary>>, [{p, CT}| St], PL, CL, Dir, Acc) ->
    case checkIfList(<<Char:8, Rest/binary>>, CL) of
        {true, Tag} ->
            BTag = makeTag(p, Dir, CT),
            Acc2 = <<Acc/binary, BTag/binary>>,
            convert(<<Char:8, Rest/binary>>, [Tag|St], PL, CL, Dir, Acc2);
        false ->
            convert(Rest, [{p, <<CT/binary, Char:8>>}|St], PL, CL, Dir, Acc)
    end;

%% Parse content
parse(<<Char:8, Rest/binary>>, [{CTag, CT}| St], PL, CL, Dir, Acc) ->
    PState = [{CTag, <<CT/binary, Char:8>>}|St],
    convert(Rest, PState, PL, CL, Dir, Acc);

%% Text end by end of data
parse(<<>>, [{_Tag, <<>>}], _PL, _CL, _Dir, Acc) ->
    Acc;

parse(<<>>, [{p, CT}| St], PL, CL, Dir, Acc) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            {CTags, _} = closeTags(St),
            BTag       = makeTag(Tag, Dir, Hdr),
            <<Acc/binary, BTag/binary, CTags/binary>>;
        false ->
            {CTags, _} = closeTags(St),
            BTag       = makeTag(p, Dir, <<CT/binary>>),
            <<Acc/binary, BTag/binary, CTags/binary>>
    end;

parse(<<>>, PState = [{Tag, _, CT}| _], _PL, _CL, Dir, Acc)
  when (Tag == ul) or (Tag == ol) ->
    {CTags, _} = closeTags(PState),
    BTag       = makeTag(li, Dir, CT),
    <<Acc/binary, BTag/binary, CTags/binary>>;

parse(<<>>, [{url, CT}| St], PL, CL, Dir, Acc) ->
    {Tag, Num, Bef, TL} = getCT(St),
    case Num of
        undefined ->
            parse(CT, [{Tag, <<Bef/binary,  $<>>}|TL], PL, CL, Dir, Acc);
        Num ->
            parse(CT, [{Tag, Num, <<Bef/binary,  $<>>}|TL], PL, CL, Dir, Acc)
    end;

parse(<<>>, [{lDesc, _Num, CT}| St], PL, CL, Dir, Acc) ->
    convert(CT, addCT(<<"[">>, St), PL, CL, Dir, Acc);

parse(<<>>, [{link, _Num, CT}, {lDesc, PT}| St], PL, CL, Dir, Acc) ->
    convert(<<PT/binary, $], $(, CT/binary>>,
            addCT(<<"[">>, St), PL, CL, Dir, Acc);

parse(<<>>, [{ilDesc, _Num, CT}| St], PL, CL, Dir, Acc) ->
    convert(CT, addCT(<<"![">>, St), PL, CL, Dir, Acc);

parse(<<>>, [{ilink, _Num, CT}, {ilDesc, PT}| St], PL, CL, Dir, Acc) ->
    convert(<<PT/binary, $], $(, CT/binary>>,
            addCT(<<"![">>, St), PL, CL, Dir, Acc);

parse(<<>>, [{Tag, CT}| St], _PL, _CL, Dir, Acc) ->
    {CTags, _} = closeTags(St),
    BTag       = makeTag(Tag, Dir, CT),
    <<Acc/binary, BTag/binary, CTags/binary>>;

%% Parse Lists
parse(Content, [ol_start| St], PL, CL, Dir, Acc) ->
    {true, NumBin, Rest} = parseOLNum(Content),
    Indent = calcIndent(CL, Content, Rest),
    Acc2   = <<Acc/binary, "<ol start='", NumBin/binary, "'>">>,
    convert(Rest, [{ol, Indent, <<>>}|St], PL, CL, Dir, Acc2);

parse(Content, [ul_start| St], PL, CL, Dir, Acc) ->
    {true, Rest} = parseULBullet(Content),
    Indent = calcIndent(CL, Content, Rest),
    Acc2   = <<Acc/binary, "<ul>">>,
    convert(Rest, [{ul, Indent, <<>>}|St], PL, CL, Dir, Acc2);

parse(<<10, Rest/binary>>, PState = [{Tag, Ind, CT}| St], PL, CL, Dir, Acc)
  when (Tag == ol) or (Tag == ul) ->
    BTag = makeTag(li, Dir, CT),
    case {Tag, parseList(Tag, Rest, Ind)} of
        {_, {true, Rest2}} ->
            Indent2 = calcIndent(CL, Rest, Rest2),
            checkIndent(ul, Indent2, <<>>, Rest2, BTag, PState, PL, CL, Dir, Acc);
        {_, {true, Num, Rest2}} ->
            Indent2 = calcIndent(CL, Rest, Rest2),
            checkIndent(ol, Indent2, Num, Rest2, BTag, PState, PL, CL, Dir, Acc);
        {ul, false} ->
            convert(Rest, St, PL, CL, Dir, <<Acc/binary, BTag/binary, "</ul>">>);
        {ol, false} ->
            convert(Rest, St, PL, CL, Dir, <<Acc/binary, BTag/binary, "</ol>">>);
        {Tag, {cont, Rest2}} ->
            convert(Rest2, [{Tag, Ind, <<CT/binary, 10>>}|St], PL, CL, Dir, Acc)
    end;
parse(<<Char:8, Rest/binary>>, [{Tag, Ind, CT}| St], PL, CL, Dir, Acc)
  when (Tag == ol) or (Tag == ul) ->
    convert(Rest, [{Tag, Ind, <<CT/binary, Char:8>>}|St], PL, CL, Dir, Acc).

%%%-------------------------------------------------------------------
%%% Different help functions
%%%-------------------------------------------------------------------

%% Check indentation of ordered and unorded lists.
checkIndent(_, Indent, _, Rest, BTag, [{Tag, Indent, CT}|St], PL, CL, Dir, Acc) ->
    BTag = makeTag(li, Dir, CT),
    convert(Rest, [{Tag, Indent, <<>>}|St], PL, CL, Dir, <<Acc/binary, BTag/binary>>);
checkIndent(Tag, Ind1, Num, Rest, BTag, [{PTag, Ind2, CT}|St], PL, CL, Dir, Acc) ->
    case Ind1 > Ind2 of
        true ->
            BTag     = makeTag(li, Dir, CT),
            NewState = [{Tag, Ind1, <<>>}, {PTag, Ind2, <<>>}|St],
            StartTag = makeStartTag(Tag, Num),
            Acc2     = <<Acc/binary, BTag/binary, StartTag/binary>>,
            convert(Rest, NewState, PL, CL, Dir, Acc2);
        false ->
            {CTags, PState} = closeTags(Ind1, [{PTag, Ind2, CT}|St]),
            Acc2 = <<Acc/binary, BTag/binary, CTags/binary>>,
            convert(Rest, PState, PL, CL, Dir, Acc2)
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

makeTag(code, _Dir, Content) ->
    Content2 = entities(Content),
    case remWS(Content2) of
        {<<>>, _} ->
            <<>>;
        {Cont3, _} ->
            <<"<pre><code>", Cont3/binary, "</code></pre>">>
    end;
makeTag(Tag, Dir, Content) ->
    BTag     = list_to_binary(atom_to_list(Tag)),
    Content2 = insertBR(Content),
    Content3 = insertEM(Content2),
    case remWS(Content3) of
        {<<>>, _} ->
            <<>>;
        {Cont4, _} ->
            Cont5 = smiley(Tag, Dir, Cont4),
            <<$<, BTag/binary, $>, Cont5/binary, $<, $/, BTag/binary, $>>>
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
headerEnd(<<10, Rest/binary>>, _) ->
    {true, <<10, Rest/binary>>};
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

insertBR(<<32, 32, 10, Rest/binary>>, SoFar) ->
    evalInsertBR(Rest, SoFar);
insertBR(<<"\\", 10, Rest/binary>>, SoFar) ->
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
            insertBR(Rest2, <<SoFar/binary, ?brTag, 10>>)
    end.

remWS(Content) ->
    remWS(Content, infinity, 0, all).

remLS(Content) ->
    remWS(Content, infinity, 0, true).

remInd(Content) ->
    remWS(Content, 4, 0, true).

remWS(<<>>,                    _, C, _)      -> {<<>>, C};
remWS(Rest, Ind, Count, _) when Count >= Ind -> {Rest, Count};
remWS(<<10, Rest/binary>>, I, C, false)  -> remWS(Rest, I, C,     true);
remWS(<<13, Rest/binary>>, I, C, false)  -> remWS(Rest, I, C,     true);
remWS(<<10, Rest/binary>>, I, C, all)    -> remWS(Rest, I, C,     all);
remWS(<<13, Rest/binary>>, I, C, all)    -> remWS(Rest, I, C,     all);
remWS(<<32, Rest/binary>>, I, C, LRem)   -> remWS(Rest, I, C + 1, LRem);
remWS(<<9,  Rest/binary>>, I, C, LRem)   -> remWS(Rest, I, C + 4, LRem);
remWS(Rest,                    _, C, _)      -> {Rest, C}.

insertEM(Content) ->
    insertEM(Content, [], <<>>).

insertEM(<<>>, [], Acc) ->
    Acc;

insertEM(<<Char:8, $*, $*, $*, Rest/binary>>, [{strong3, CT}|St], Acc)
    when (Char =/= 32) and (Char =/= 10) and (Char =/= 13) and (Char =/= 9)
    and  (Char =/= $*) ->
    BoldText = <<"<strong>", CT/binary, Char:8, "</strong>">>,
    insertEM(Rest, St, <<Acc/binary, BoldText/binary>>);

insertEM(<<Char:8, $*, $*, Rest/binary>>, [{strong, CT}|St], Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9) ->
    BoldText = <<"<strong>", CT/binary, Char:8, "</strong>">>,
    insertEM(Rest, St, <<Acc/binary, BoldText/binary>>);

insertEM(<<Char:8, $*, Rest/binary>>, [{em, CT}|St], Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9) ->
    BoldText = <<"<em>", CT/binary, Char:8, "</em>">>,
    insertEM(Rest, St, <<Acc/binary, BoldText/binary>>);

insertEM(<<Char:8, $_, $_, $_, Rest/binary>>, [{strong4, CT}|St], Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9)
    and  (Char =/= $_) ->
    BoldText = <<"<strong>", CT/binary, Char:8, "</strong>">>,
    insertEM(Rest, St, <<Acc/binary, BoldText/binary>>);

insertEM(<<Char:8, $_, $_, Rest/binary>>, [{strong2, CT}|St], Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9) ->
    BoldText = <<"<strong>", CT/binary, Char:8, "</strong>">>,
    insertEM(Rest, St, <<Acc/binary, BoldText/binary>>);

insertEM(<<Char:8, $_, Rest/binary>>, [{em2, CT}|St], Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9) ->
    BoldText = <<"<em>", CT/binary, Char:8, "</em>">>,
    insertEM(Rest, St, <<Acc/binary, BoldText/binary>>);

insertEM(<<$*, $*, $*, Char:8, Rest/binary>>, St, Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9) ->
    insertEM(Rest, [{strong3, <<Char:8>>}|St], Acc);

insertEM(<<$*, $*, Char:8, Rest/binary>>, St, Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9)
    and  (Char =/= $*) ->
    insertEM(Rest, [{strong, <<Char:8>>}|St], Acc);

insertEM(<<$*, Char:8, Rest/binary>>, St, Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9)
    and  (Char =/= $*) ->
    insertEM(Rest, [{em, <<Char:8>>}|St], Acc);

insertEM(<<$_, $_, $_, Char:8, Rest/binary>>, St, Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9) ->
    insertEM(Rest, [{strong4, <<Char:8>>}|St], Acc);

insertEM(<<$_, $_, Char:8, Rest/binary>>, St, Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9)
    and  (Char =/= $_) ->
    insertEM(Rest, [{strong2, <<Char:8>>}|St], Acc);

insertEM(<<$_, Char:8, Rest/binary>>, St, Acc)
    when (Char =/= 32) and  (Char =/= 10) and (Char =/= 13) and (Char =/= 9)
    and  (Char =/= $_) ->
    insertEM(Rest, [{em2, <<Char:8>>}|St], Acc);

insertEM(<<>>, [{em, CT}|St], Acc) ->
    insertEM(<<>>, St, <<Acc/binary, $*, CT/binary>>);

insertEM(<<>>, [{em2, CT}|St], Acc) ->
    insertEM(<<>>, St, <<Acc/binary, $_, CT/binary>>);

insertEM(<<>>, [{strong, CT}|St], Acc) ->
    insertEM(<<>>, St, <<Acc/binary, $*, $*, CT/binary>>);

insertEM(<<>>, [{strong2, CT}|St], Acc) ->
    insertEM(<<>>, St, <<Acc/binary, $_, $_, CT/binary>>);

insertEM(<<>>, [{strong3, CT}|St], Acc) ->
    insertEM(<<>>, St, <<Acc/binary, $*, $*, $*, CT/binary>>);

insertEM(<<>>, [{strong4, CT}|St], Acc) ->
    insertEM(<<>>, St, <<Acc/binary, $_, $_, $_, CT/binary>>);

insertEM(<<Char:8, Rest/binary>>, [{Tag, CT}|St], Acc) ->
    insertEM(Rest, [{Tag, <<CT/binary, Char:8>>}|St], Acc);

insertEM(<<Char:8, Rest/binary>>, St, Acc) ->
    insertEM(Rest, St, <<Acc/binary, Char:8>>).

checkIfList(Content, CL) ->
    case getCurrentRow(Content) of
        CL -> %% Check if the list is at the beginning of the row.
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
            end;
        _ ->
        false
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

getCT(PState = [{Tag, Num, _CT}|TL]) ->
    {Tag, Num, getCT(PState, <<>>), TL};
getCT(PState = [{Tag, _CT}|TL]) ->
    {Tag, undefined, getCT(PState, <<>>), TL};
getCT([]) ->
    {p, undefined, <<>>, []}.

getCT([], Acc) ->
    Acc;
getCT([{_Tag, CT}|St], Acc) ->
    getCT(St, <<Acc/binary, CT/binary>>);
getCT([{_Tag, _, CT}|St], Acc) ->
    getCT(St, <<Acc/binary, CT/binary>>).


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

smiley(Tag, Dir, Text) ->
    smiley(Tag, Text, Dir, <<>>).

smiley(_Tag, <<>>, _Dir, SoFar) ->
    SoFar;

%% <3
smiley(Tag, <<"<3", Rest/binary>>, Dir, SoFar) ->
    Heart = smileyIcon(Tag, Dir, <<"heart.png">>, <<"<3">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Heart/binary>>);

%% :-) :) =) :] :>
smiley(Tag, <<":-)", Rest/binary>>, Dir, SoFar) ->
    Happy = smileyIcon(Tag, Dir, <<"happy.png">>, <<":-)">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Happy/binary>>);
smiley(Tag, <<":)", Rest/binary>>, Dir, SoFar) ->
    Happy = smileyIcon(Tag, Dir, <<"happy.png">>, <<":)">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Happy/binary>>);
smiley(Tag, <<"=)", Rest/binary>>, Dir, SoFar) ->
    Happy = smileyIcon(Tag, Dir, <<"happy.png">>, <<"=)">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Happy/binary>>);
smiley(Tag, <<":]", Rest/binary>>, Dir, SoFar) ->
    Happy = smileyIcon(Tag, Dir, <<"happy.png">>, <<":]">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Happy/binary>>);
smiley(Tag, <<":>", Rest/binary>>, Dir, SoFar) ->
    Happy = smileyIcon(Tag, Dir, <<"happy.png">>, <<":>">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Happy/binary>>);

%% :-(  :( =( :[ :C
smiley(Tag, <<":-(", Rest/binary>>, Dir, SoFar) ->
    Sad = smileyIcon(Tag, Dir, <<"sad.png">>, <<":-(">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Sad/binary>>);
smiley(Tag, <<":(", Rest/binary>>, Dir, SoFar) ->
    Sad = smileyIcon(Tag, Dir, <<"sad.png">>, <<":(">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Sad/binary>>);
smiley(Tag, <<"=(", Rest/binary>>, Dir, SoFar) ->
    Sad = smileyIcon(Tag, Dir, <<"sad.png">>, <<"=(">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Sad/binary>>);
smiley(Tag, <<":[", Rest/binary>>, Dir, SoFar) ->
    Sad = smileyIcon(Tag, Dir, <<"sad.png">>, <<":[">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Sad/binary>>);
smiley(Tag, <<":C", Rest/binary>>, Dir, SoFar) ->
    Sad = smileyIcon(Tag, Dir, <<"sad.png">>, <<":C">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Sad/binary>>);

%% ;-)  ;)  ^.~
smiley(Tag, <<";-)", Rest/binary>>, Dir, SoFar) ->
    Wink = smileyIcon(Tag, Dir, <<"wink.png">>, <<";-)">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Wink/binary>>);
smiley(Tag, <<";)", Rest/binary>>, Dir, SoFar) ->
    Wink = smileyIcon(Tag, Dir, <<"wink.png">>, <<";)">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Wink/binary>>);
smiley(Tag, <<"^.~", Rest/binary>>, Dir, SoFar) ->
    Wink = smileyIcon(Tag, Dir, <<"wink.png">>, <<"^.~">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Wink/binary>>);

%% =D :D
smiley(Tag, <<"=D", Rest/binary>>, Dir, SoFar) ->
    LOL = smileyIcon(Tag, Dir, <<"lol.png">>, <<"=D">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, LOL/binary>>);
smiley(Tag, <<":D", Rest/binary>>, Dir, SoFar) ->
    LOL = smileyIcon(Tag, Dir, <<"lol.png">>, <<":D">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, LOL/binary>>);

%% =O :O :-O
smiley(Tag, <<"=O", Rest/binary>>, Dir, SoFar) ->
    Shock = smileyIcon(Tag, Dir, <<"shocked.png">>, <<"=O">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Shock/binary>>);
smiley(Tag, <<":O", Rest/binary>>, Dir, SoFar) ->
    Shock = smileyIcon(Tag, Dir, <<"shocked.png">>, <<":O">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Shock/binary>>);
smiley(Tag, <<":-O", Rest/binary>>, Dir, SoFar) ->
    Shock = smileyIcon(Tag, Dir, <<"shocked.png">>, <<":-O">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Shock/binary>>);

%% =P :P
smiley(Tag, <<"=P", Rest/binary>>, Dir, SoFar) ->
    Mischief = smileyIcon(Tag, Dir, <<"mischief.png">>, <<"=P">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Mischief/binary>>);
smiley(Tag, <<":P", Rest/binary>>, Dir, SoFar) ->
    Mischief = smileyIcon(Tag, Dir, <<"mischief.png">>, <<":P">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Mischief/binary>>);

%% :,(
smiley(Tag, <<":,(", Rest/binary>>, Dir, SoFar) ->
    Cry = smileyIcon(Tag, Dir, <<"crying.png">>, <<":,(">>),
    smiley(Tag, Rest, Dir, <<SoFar/binary, Cry/binary>>);

smiley(Tag, <<Char:8, Rest/binary>>, Dir, SoFar) ->
    smiley(Tag, Rest, Dir, <<SoFar/binary, Char:8>>).

smileyIcon(Tag, Dir, Icon, Txt) ->
    Options = setOptions(Tag),
    <<"&nbsp;<img class='emote' alt='",
      Txt/binary, "' src='", Dir/binary, "/Icons/", Icon/binary, "' ",
      Options/binary, " />&nbsp;">>.

setOptions(h1) ->
    <<"align=bottom height='25' width='25'">>;
setOptions(h2) ->
    <<"align=bottom height='22' width='22'">>;
setOptions(h3) ->
    <<"align=bottom height='19' width='19'">>;
setOptions(_Tag) ->
    <<"align=bottom">>.

removeParam(CT) ->
    removeParam(CT, CT, 0, <<>>).

removeParam(<<$(, CT/binary>>, NotModified, 0, SoFar) ->
    removeParam(CT, NotModified, 1, SoFar);
removeParam(<<$), CT/binary>>, NotModified, 1, SoFar) ->
    removeParam(CT, NotModified, 0, SoFar);
removeParam(<<Char:8, CT/binary>>, NotModified, Num, SoFar) ->
    removeParam(CT, NotModified, Num, <<SoFar/binary, Char:8>>);
removeParam(<<>>, _NotModified, 0, SoFar) ->
    SoFar;
removeParam(<<>>, NotModified, _Num, _SoFar) ->
    NotModified.

%%%-------------------------------------------------------------------
%%% ConvertLineEnding
%%%-------------------------------------------------------------------

convertLineEnding(Text, Type) ->
    convertLineEnding(Text, Type, <<>>).

convertLineEnding(<<>>, _Type, SoFar) ->
    SoFar;
convertLineEnding(<<13, 10, Rest/binary>>, crlf2lf, SoFar) ->
    convertLineEnding(Rest, crlf2lf, <<SoFar/binary, 10>>);
convertLineEnding(<<10, Rest/binary>>, lf2crlf, SoFar) ->
    convertLineEnding(Rest, lf2crlf, <<SoFar/binary, 13, 10>>);
convertLineEnding(<<Char:8, Rest/binary>>, Type, SoFar) ->
    convertLineEnding(Rest, Type, <<SoFar/binary, Char:8>>).
