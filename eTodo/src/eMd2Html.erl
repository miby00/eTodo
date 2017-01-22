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

convert(MDText) when is_list(MDText) ->
    convert(unicode:characters_to_binary(MDText));
convert(MDText) when is_binary(MDText) ->
    convert(MDText, #{state     => ptext,
                      currToken => <<>>,
                      prevLine  => <<>>,
                      currLine  => <<>>,
                      soFar     => <<>>}).

%% Support systems with only \n.
convert(<<10, 10, Rest/binary>>, State) ->
    convert(<<13, 10, 13, 10, Rest/binary>>, State);
convert(<<10, Rest/binary>>, State) ->
    convert(<<13, 10, Rest/binary>>, State);

%% Headers
convert(<<"#", Rest/binary>>, State = #{state     := ptext,
                                        currLine  := CL,
                                        currToken := CT,
                                        soFar     := Html})
  when (CL == <<>>)     or
       (CL == <<" ">>)  or
       (CL == <<"  ">>) or
       (CL == <<"   ">>) ->
    case headerStart(<<"#", Rest/binary>>) of
        {true, Tag, Rest2} ->
            BinTag = makeTag(p, CT),
            Html2  = <<Html/binary, BinTag/binary>>,
            convert(Rest2, State#{state     := Tag,
                                  currToken := <<>>,
                                  soFar     := Html2});
        false ->
            convert(Rest, State#{currToken := <<CT/binary, "#">>})
    end;

%% Header end by end of data
convert(<<>>, #{state     := Tag,
                currToken := CT,
                prevLine  := <<>>,
                soFar     := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, BinTag/binary>>;
convert(<<>>, #{state     := Tag,
                currToken := CT,
                prevLine  := LL,
                soFar     := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, LL/binary, ?brTag, BinTag/binary>>;

%% Header end by #...
convert(<<" #", Rest/binary>>, State = #{state       := Tag,
                                         currToken   := CT,
                                         soFar := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    case headerEnd(Rest) of
        {true, Rest2} ->
            convert(<<Rest2/binary>>, State#{state     := Tag,
                                             currToken := CT,
                                             soFar     := Html});
        false ->
            convert(Rest, State#{currToken := <<CT/binary, " #">>})
    end;

%% Header end by line end.
convert(<<13, 10, Rest/binary>>, State = #{state     := Tag,
                                           currToken := CT,
                                           soFar     := Html})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(Rest, State#{state     := ptext,
                         currToken := <<>>,
                         soFar     := Html2});

%% Parse header paragraf text
convert(<<Char:8, Rest/binary>>, State = #{state     := Tag,
                                           currToken := CT})
  when (Tag == h6) or (Tag == h5) or (Tag == h4) or
       (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    convert(Rest, State#{currToken := <<CT/binary, Char:8>>});

%% Paragraf text ending
convert(<<13, 10, 13, 10, Rest/binary>>, State = #{state     := ptext,
                                                   currToken := CT,
                                                   prevLine  := <<>>,
                                                   soFar     := Html}) ->
    BinTag = makeTag(p, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(Rest, State#{currLine  := <<>>,
                         currToken := <<>>,
                         soFar     := Html2});
%% Paragraf line ending, empty previous line
convert(<<13, 10, Rest/binary>>, State = #{state     := ptext,
                                           currToken := CT,
                                           currLine  := CL,
                                           prevLine  := <<>>}) ->
    convert(Rest, State#{prevLine  := <<CL/binary>>,
                         currLine  := <<>>,
                         currToken := <<CT/binary, 13, 10>>});
convert(<<13, 10, Rest/binary>>, State = #{state     := ptext,
                                           prevLine  := PL,
                                           currLine  := CL,
                                           currToken := CT,
                                           soFar     := Html}) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BinTag = makeTag(Tag, Hdr),
            Html2  = <<Html/binary, BinTag/binary, 13, 10>>,
            convert(Rest, State#{soFar     := Html2,
                                 prevLine  := <<>>,
                                 currLine  := <<>>,
                                 currToken := <<>>});
        false ->
            case Rest of
                <<13, 10, Rest2/binary>> ->
                    BinTag = makeTag(p, CT),
                    Html2  = <<Html/binary, BinTag/binary>>,
                    convert(Rest2, State#{prevLine  := <<>>,
                                          currLine  := <<>>,
                                          currToken := <<>>,
                                          soFar     := Html2});
                Rest ->
                    convert(Rest, State#{soFar     := Html,
                                         prevLine  := CL,
                                         currLine  := <<>>,
                                         currToken := <<CT/binary, 13, 10>>})
            end
    end;

%% Check for list
convert(<<Char:8, Rest/binary>>, State = #{state     := ptext,
                                           currLine  := <<>>,
                                           currToken := CT,
                                           soFar     := Html}) ->
    case checkIfList(<<Char:8, Rest/binary>>) of
        {true, Tag} ->
            BinTag = makeTag(p, CT),
            Html2  = <<Html/binary, BinTag/binary>>,
            convert(<<Char:8, Rest/binary>>, State#{state     := Tag,
                                                    prevLine  := <<>>,
                                                    currToken := <<>>,
                                                    soFar     := Html2});
        false ->
            convert(Rest, State#{currLine  := <<Char:8>>,
                                 currToken := <<CT/binary, Char:8>>})
    end;

%% Parse ptext
convert(<<Char:8, Rest/binary>>, State = #{state     := ptext,
                                           currLine  := CL,
                                           currToken := CT}) ->
    convert(Rest, State#{currLine  := <<CL/binary, Char:8>>,
                         currToken := <<CT/binary, Char:8>>});

%% Text end by end of data
convert(<<>>, #{state     := ptext,
                currToken := <<>>,
                soFar     := Html}) ->
    Html;
convert(<<>>, #{state     := ptext,
                prevLine  := <<>>,
                currToken := CT,
                soFar     := Html}) ->
    BinTag = makeTag(p, <<CT/binary>>),
    <<Html/binary, BinTag/binary>>;
convert(<<>>, #{state     := ptext,
                prevLine  := PL,
                currLine  := CL,
                currToken := CT,
                soFar     := Html}) ->
    case header(CL, PL) of
        {true, Tag, Hdr} ->
            BinTag = makeTag(Tag, Hdr),
            <<Html/binary, BinTag/binary>>;
        false ->
            BinTag = makeTag(p, <<CT/binary>>),
            <<Html/binary, BinTag/binary>>
    end;
convert(Content, State = #{state := ol_start,
                           soFar := Html}) ->
    {true, NumBin, Rest} = parseOLNum(Content),
    Html2 = <<Html/binary, "<ol start='", NumBin/binary, "'>">>,
    convert(Rest, State#{state := ol_litem, soFar := Html2});
convert(<<>>, #{state     := ol_litem,
                currToken := CT,
                soFar     := Html}) ->
    BinTag = makeTag(li, CT),
    <<Html/binary, BinTag/binary, "</ol>">>;
convert(<<13, 10, Rest/binary>>, State = #{state     := ol_litem,
                                           currToken := CT,
                                           soFar     := Html}) ->
    BinTag = makeTag(li, CT),
    case parseOLNum(Rest) of
        {true, _Num, Rest2} ->
            convert(Rest2, State#{currToken := <<>>,
                                  soFar     := <<Html/binary, BinTag/binary>>});
        false ->
            Html2 = <<Html/binary, BinTag/binary, "</ol>">>,
            convert(Rest, State#{state     := ptext,
                                 currToken := <<>>,
                                 soFar     := Html2})
    end;
convert(<<Char:8, Rest/binary>>, State = #{state     := ol_litem,
                                           currToken := CT}) ->
    convert(Rest, State#{currToken := <<CT/binary, Char:8>>});
convert(Content, State = #{state := ul_start,
                           soFar := Html}) ->
    {true, Rest} = parseULBullet(Content),
    Html2 = <<Html/binary, "<ul>">>,
    convert(Rest, State#{state := ul_litem, soFar := Html2});
convert(<<>>, #{state     := ul_litem,
                currToken := CT,
                soFar     := Html}) ->
    BinTag = makeTag(li, CT),
    <<Html/binary, BinTag/binary, "</ul>">>;
convert(<<13, 10, Rest/binary>>, State = #{state     := ul_litem,
                                           currToken := CT,
                                           soFar     := Html}) ->
    BinTag = makeTag(li, CT),
    case parseULBullet(Rest) of
        {true, Rest2} ->
            convert(Rest2, State#{currToken := <<>>,
                                  soFar     := <<Html/binary, BinTag/binary>>});
        false ->
            Html2 = <<Html/binary, BinTag/binary, "</ul>">>,
            convert(Rest, State#{state     := ptext,
                                 currToken := <<>>,
                                 soFar     := Html2})
    end;
convert(<<Char:8, Rest/binary>>, State = #{state     := ul_litem,
                                           currToken := CT}) ->
    convert(Rest, State#{currToken := <<CT/binary, Char:8>>}).

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
    insertBR(Rest, <<SoFar/binary, ?brTag>>);
insertBR(<<32, Rest/binary>>, <<>>) ->
    insertBR(Rest, <<>>);
insertBR(<<Char:8, Rest/binary>>, SoFar) ->
    insertBR(Rest, <<SoFar/binary, Char:8>>);
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
    case remBegWS(Content) of
        Content2 when (byte_size(Content) - byte_size(Content2)) > 3 ->
            false;
        Content2 ->
            doParseULBullet(Content2)
    end.

doParseULBullet(<<"* ", Rest/binary>>) -> {true, remBegWS(Rest)};
doParseULBullet(<<"- ", Rest/binary>>) -> {true, remBegWS(Rest)};
doParseULBullet(<<"+ ", Rest/binary>>) -> {true, remBegWS(Rest)};
doParseULBullet(_Content)              -> false.

parseOLNum(Content) ->
    case remBegWS(Content) of
        Content2 when (byte_size(Content) - byte_size(Content2)) > 3 ->
            false;
        Content2 ->
            parseOLNum(Content2, <<>>)
    end.

parseOLNum(<<Num:8, Rest/binary>>, SoFar)
  when ((Num >= $0) and (Num =< $9)) and (byte_size(SoFar) < 10) ->
    parseOLNum(Rest, <<SoFar/binary, Num:8>>);
parseOLNum(<<". ", Rest/binary>>, SoFar) ->
    {true, SoFar, remBegWS(Rest)};
parseOLNum(_Content, _SoFar) ->
    false.




