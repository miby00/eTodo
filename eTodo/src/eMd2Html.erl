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

%% State needs to be a list.

convert(MDText) when is_list(MDText) ->
    convert(unicode:characters_to_binary(MDText));
convert(MDText) when is_binary(MDText) ->
    convert(MDText, #{state    => [],
                      prevLine => <<>>,
                      currLine => <<>>,
                      soFar    => <<>>}).

%% Support systems with only \n.
convert(<<10, 10, Rest/binary>>, State) ->
    convert(<<13, 10, 13, 10, Rest/binary>>, State);
convert(<<10, Rest/binary>>, State) ->
    convert(<<13, 10, Rest/binary>>, State);

%% Default state is ptext
convert(Content, State = #{state := []}) ->
    convert(Content, State#{state := [{ptext, <<>>}]});

%% URL
convert(<<$<, Rest/binary>>, State = #{state := ST}) ->
    convert(Rest, State#{state := [{url, <<>>}|ST]});

convert(<<$>, Rest/binary>>, State = #{state := [{url, CT}|St]}) ->
    case catch http_uri:parse(binary_to_list(CT)) of
        {error, _} ->
            convert(<<CT/binary, $>, Rest/binary>>,
                    State#{state := addCT(<<"<">>, St)});
        _ ->
            Url = <<"<a href='", CT/binary, "'>", CT/binary, "</a>">>,
            convert(Rest, State#{state := addCT(Url, St)})
    end;
convert(<<Char:8, Rest/binary>>, State = #{state := PState = [{url, _}|_]}) ->
    convert(Rest, State#{state := addCT(<<Char:8>>, PState)});

%% Headers
convert(<<"#", Rest/binary>>, State = #{state    := [{ptext, CT}|St],
                                        currLine := CL,
                                        soFar    := Html})
  when (CL == <<>>)     or
       (CL == <<" ">>)  or
       (CL == <<"  ">>) or
       (CL == <<"   ">>) ->
    case headerStart(<<"#", Rest/binary>>) of
        {true, Tag, Rest2} ->
            BinTag = makeTag(p, CT),
            Html2  = <<Html/binary, BinTag/binary>>,
            convert(Rest2, State#{state := [{Tag, <<>>}|St],
                                  soFar := Html2});
        false ->
            convert(Rest, State#{state := [{ptext, <<CT/binary, "#">>}|St]})
    end;

%% Header end by end of data
convert(<<>>, #{state     := [{Tag, CT}|_],
                prevLine  := <<>>,
                soFar     := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, BinTag/binary>>;
convert(<<>>, #{state     := [{Tag, CT}|_],
                prevLine  := LL,
                soFar     := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, LL/binary, ?brTag, BinTag/binary>>;

%% Header end by #...
convert(<<" #", Rest/binary>>, State = #{state := [{Tag, CT}|St],
                                         soFar := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    case headerEnd(Rest) of
        {true, Rest2} ->
            convert(<<Rest2/binary>>, State#{state := [{Tag, CT}|St],
                                             soFar := Html});
        false ->
            convert(Rest, State#{start := [{Tag, <<CT/binary, " #">>}|St]})
    end;

%% Header end by line end.
convert(<<13, 10, Rest/binary>>, State = #{state := [{Tag, CT}|St],
                                           soFar := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(Rest, State#{state := St,
                         soFar := Html2});

%% Parse header paragraf text
convert(<<Char:8, Rest/binary>>, State = #{state := [{Tag, CT}|St]})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    convert(Rest, State#{state := [{Tag, <<CT/binary, Char:8>>}|St]});

%% Paragraf text ending
convert(<<13, 10, 13, 10, Rest/binary>>, State = #{state    := [{ptext, CT}|St],
                                                   prevLine := <<>>,
                                                   soFar    := Html}) ->
    BinTag = makeTag(p, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(Rest, State#{state    := St,
                         currLine := <<>>,
                         soFar    := Html2});
%% Paragraf line ending, empty previous line
convert(<<13, 10, Rest/binary>>, State = #{state    := [{ptext, CT}|St],
                                           currLine := CL,
                                           prevLine := <<>>}) ->
    convert(Rest, State#{prevLine := <<CL/binary>>,
                         currLine := <<>>,
                         state    := [{ptext, <<CT/binary, 13, 10>>}|St]});
convert(<<13, 10, Rest/binary>>, State = #{state    := [{ptext, CT}|St],
                                           prevLine := PL,
                                           currLine := CL,
                                           soFar    := Html}) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BinTag = makeTag(Tag, Hdr),
            Html2  = <<Html/binary, BinTag/binary, 13, 10>>,
            convert(Rest, State#{state    := St,
                                 soFar    := Html2,
                                 prevLine := <<>>,
                                 currLine := <<>>});
        false ->
            case Rest of
                <<13, 10, Rest2/binary>> ->
                    BinTag = makeTag(p, CT),
                    Html2  = <<Html/binary, BinTag/binary>>,
                    convert(Rest2, State#{state    := St,
                                          prevLine := <<>>,
                                          currLine := <<>>,
                                          soFar    := Html2});
                Rest ->
                    State2 = [{ptext, <<CT/binary, 13, 10>>}|St],
                    convert(Rest, State#{soFar    := Html,
                                         prevLine := CL,
                                         currLine := <<>>,
                                         state    := State2})
            end
    end;

%% Check for list
convert(<<Char:8, Rest/binary>>, State = #{state    := [{ptext, CT}|St],
                                           currLine := <<>>,
                                           soFar    := Html}) ->

    case checkIfList(<<Char:8, Rest/binary>>) of
        {true, Tag} ->
            BinTag = makeTag(p, CT),
            Html2  = <<Html/binary, BinTag/binary>>,
            convert(<<Char:8, Rest/binary>>,
                    State#{state    := [Tag|St],
                           prevLine := <<>>,
                           soFar    := Html2});
        false ->
            ST2 = [{ptext, <<CT/binary, Char:8>>}|St],
            convert(Rest, State#{currLine := <<Char:8>>,
                                 state    := ST2})
    end;

%% Parse ptext
convert(<<Char:8, Rest/binary>>, State = #{state    := [{ptext, CT}|St],
                                           currLine := CL}) ->
    convert(Rest, State#{currLine := <<CL/binary, Char:8>>,
                         state    := [{ptext, <<CT/binary, Char:8>>}|St]});

%% Text end by end of data
convert(<<>>, #{state := [{ptext, <<>>}|_],
                soFar := Html}) ->
    Html;
convert(<<>>, #{state    := [{ptext, CT}|_],
                prevLine := <<>>,
                soFar    := Html}) ->
    BinTag = makeTag(p, <<CT/binary>>),
    <<Html/binary, BinTag/binary>>;
convert(<<>>, #{state    := [{ptext, CT}|_],
                prevLine := PL,
                currLine := CL,
                soFar    := Html}) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BinTag = makeTag(Tag, Hdr),
            <<Html/binary, BinTag/binary>>;
        false ->
            BinTag = makeTag(p, <<CT/binary>>),
            <<Html/binary, BinTag/binary>>
    end;
convert(Content, State = #{state := [ol_start|St],
                           soFar := Html}) ->
    {true, NumBin, Rest} = parseOLNum(Content),
    Indent = byte_size(Content) - byte_size(Rest),
    Html2 = <<Html/binary, "<ol start='", NumBin/binary, "'>">>,
    convert(Rest, State#{state := [{ol_litem, Indent, <<>>}|St], soFar := Html2});
convert(<<>>, #{state     := PState = [{ol_litem, _, CT}|_],
                soFar     := Html}) ->
    BinTag = makeTag(li, CT),
    {CloseTags, _} = closeTags(0, PState),
    <<Html/binary, BinTag/binary, CloseTags/binary>>;
convert(<<13, 10, Rest/binary>>, State = #{state := [{ol_litem, Ind, CT}|St],
                                           soFar := Html}) ->
    BinTag = makeTag(li, CT),
    case parseOLNum(Rest, Ind) of
        {true, Num, Rest2} ->
            Indent2 = byte_size(Rest) - byte_size(Rest2),
            checkIndent(Indent2, Num, Rest2, BinTag, State);
        false ->
            Html2 = <<Html/binary, BinTag/binary, "</ol>">>,
            convert(Rest, State#{state := St,
                                 soFar := Html2})
    end;
convert(<<Char:8, Rest/binary>>, State = #{state := [{ol_litem, Ind, CT}|St]}) ->
    convert(Rest, State#{state := [{ol_litem, Ind, <<CT/binary, Char:8>>}|St]});
convert(Content, State = #{state := [ul_start|St], soFar := Html}) ->
    {true, Rest} = parseULBullet(Content),
    Indent       = byte_size(Content) - byte_size(Rest),
    Html2        = <<Html/binary, "<ul>">>,
    convert(Rest, State#{state := [{ul_litem, Indent, <<>>}|St], soFar := Html2});
convert(<<>>, #{state := PState = [{ul_litem, _, CT}|_],
                soFar := Html}) ->
    BinTag         = makeTag(li, CT),
    {CloseTags, _} = closeTags(0, PState),
    <<Html/binary, BinTag/binary, CloseTags/binary>>;
convert(<<13, 10, Rest/binary>>, State = #{state := [{ul_litem, Ind, CT}|St],
                                           soFar := Html}) ->
    BinTag = makeTag(li, CT),
    case parseULBullet(Rest, Ind) of
        {true, Rest2} ->
            Indent2 = byte_size(Rest) - byte_size(Rest2),
            checkIndent(Indent2, <<>>, Rest2, BinTag, State);
        false ->
            Html2 = <<Html/binary, BinTag/binary, "</ul>">>,
            convert(Rest, State#{state := St,
                                 soFar := Html2})
    end;
convert(<<Char:8, Rest/binary>>, State = #{state := [{ul_litem, Ind, CT}|St]}) ->
    convert(Rest, State#{state := [{ul_litem, Ind, <<CT/binary, Char:8>>}|St]}).

checkIndent(Indent, _, Rest, BinTag, State = #{state := [{Tag, Indent, CT}|St],
                                               soFar := Html}) ->
    BinTag = makeTag(li, CT),
    convert(Rest, State#{state := [{Tag, Indent, <<>>}|St],
                         soFar := <<Html/binary, BinTag/binary>>});
checkIndent(Ind1, _, Rest, BinTag, State = #{state := [{ul_litem, Ind2, CT}|St],
                                             soFar := Html}) ->
    case Ind1 > Ind2 of
        true ->
            BinTag   = makeTag(li, CT),
            NewState = [{ul_litem, Ind1, <<>>}, {ul_litem, Ind2, <<>>}|St],
            Html2    = <<Html/binary, BinTag/binary, "<ul>">>,
            convert(Rest, State#{state := NewState,
                                 soFar := Html2});
        false ->
            {CTags, PState} = closeTags(Ind1, [{ul_litem, Ind2, CT}|St]),
            Html2 = <<Html/binary, BinTag/binary, CTags/binary>>,
            convert(Rest, State#{state := PState,
                                 soFar := Html2})
    end;
checkIndent(Ind1, Num, Rest, BinTag, State = #{state := [{ol_litem, Ind2, CT}|St],
                                               soFar := Html}) ->
    case Ind1 > Ind2 of
        true ->
            BinTag   = makeTag(li, CT),
            NewState = [{ol_litem, Ind1, <<>>}, {ol_litem, Ind2, <<>>}|St],
            StartTag = <<"<ol start='", Num/binary, "'>">>,
            Html2    = <<Html/binary, BinTag/binary, StartTag/binary>>,
            convert(Rest, State#{state := NewState,
                                 soFar := Html2});
        false ->
            Html2 = <<Html/binary, BinTag/binary, "</ol>">>,
            convert(Rest, State#{state := St,
                                 soFar := Html2})
    end.

closeTags(Ind, PState) ->
    closeTags(Ind, PState, <<>>).

closeTags(_Ind, [], SoFar) ->
    {SoFar, []};
closeTags(Ind, [{ul_litem, Ind, CT}|Rest], SoFar) ->
    {SoFar, [{ul_litem, Ind, CT}|Rest]};
closeTags(Ind1, [{ul_litem, Ind2, _}|Rest], SoFar) when Ind1 < Ind2 ->
    closeTags(Ind1, Rest, <<SoFar/binary, "</ul>">>);
closeTags(Ind, [{ol_litem, Ind, CT}|Rest], SoFar) ->
    {SoFar, [{ol_litem, Ind, CT}|Rest]};
closeTags(Ind1, [{ol_litem, Ind2, _}|Rest], SoFar) when Ind1 < Ind2 ->
    closeTags(Ind1, Rest, <<SoFar/binary, "</ol>">>);
closeTags(_Ind, PState, SoFar) ->
    {SoFar, PState}.

makeTag(Tag, Content) ->
    BinTag   = list_to_binary(atom_to_list(Tag)),
    Content2 = insertBR(Content),
    case remBegWS(Content2) of
        <<>> ->
            <<>>;
        Cont3 ->
            <<$<, BinTag/binary, $>, Cont3/binary, $<, $/, BinTag/binary, $>>>
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
    case (remBegWS(Rest) == <<>>) of
        true ->
            SoFar;
        false ->
            insertBR(Rest, <<SoFar/binary, ?brTag, 13, 10>>)
    end;
insertBR(Bin = <<Char:8, Rest/binary>>, SoFar) ->
    case (remBegWS(Bin) == <<>>) of
        true ->
            SoFar;
        false ->
            insertBR(Rest, <<SoFar/binary, Char:8>>)
    end;
insertBR(<<32, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(<<9, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(<<>>, SoFar) ->
    SoFar.

remBegWS(<<>>)                -> <<>>;
remBegWS(<<13, Rest/binary>>) -> remBegWS(Rest);
remBegWS(<<10, Rest/binary>>) -> remBegWS(Rest);
remBegWS(<<32, Rest/binary>>) -> remBegWS(Rest);
remBegWS(<<9,  Rest/binary>>) -> remBegWS(Rest);
remBegWS(Content)             -> Content.

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

parseULBullet(Content) ->
    parseULBullet(Content, 0).

parseULBullet(Content, Ind) ->
    case remBegWS(Content) of
        Content2 when (byte_size(Content) - byte_size(Content2)) > (3 + Ind) ->
            false;
        Content2 ->
            doParseULBullet(Content2)
    end.

doParseULBullet(<<"* ", Rest/binary>>) -> {true, remBegWS(Rest)};
doParseULBullet(<<"- ", Rest/binary>>) -> {true, remBegWS(Rest)};
doParseULBullet(<<"+ ", Rest/binary>>) -> {true, remBegWS(Rest)};
doParseULBullet(_Content)              -> false.

parseOLNum(Content) ->
    parseOLNum(Content, 0).

parseOLNum(Content, Ind) ->
    case remBegWS(Content) of
        Content2 when (byte_size(Content) - byte_size(Content2)) > (Ind + 3) ->
            false;
        Content2 ->
            doParseOLNum(Content2, <<>>)
    end.

doParseOLNum(<<Num:8, Rest/binary>>, SoFar)
  when ((Num >= $0) and (Num =< $9)) and (byte_size(SoFar) < 10) ->
    doParseOLNum(Rest, <<SoFar/binary, Num:8>>);
doParseOLNum(<<". ", Rest/binary>>, SoFar) ->
    {true, SoFar, remBegWS(Rest)};
doParseOLNum(_Content, _SoFar) ->
    false.

addCT(D, [{Tag, CT}|St]) when is_atom(Tag), is_binary(D), is_binary(CT) ->
    [{Tag, <<CT/binary, D/binary>>}|St];
addCT(D, [{Tag, Num, CT}|St]) when is_atom(Tag), is_binary(D), is_binary(CT) ->
    [{Tag, Num, <<CT/binary, D/binary>>}|St].