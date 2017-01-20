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
    convert(MDText, #{state        => text,
                      currentToken => <<>>,
                      lastLine     => <<>>,
                      html         => <<>>,
                      rest         => <<>>}).

%% Support systems with only \n.
convert(<<10, Rest/binary>>, State) ->
    convert(<<13, 10, Rest/binary>>, State);

%% Headers
convert(<<"#", Rest/binary>>, State = #{state := text, currentToken := CT})
    when (CT == <<>>)     or
         (CT == <<" ">>)  or
         (CT == <<"  ">>) or
         (CT == <<"   ">>) ->
    case headerStart(<<"#", Rest/binary>>) of
        {true, Tag, Rest2} ->
            convert(Rest2, State#{state        := Tag,
                                  currentToken :=  <<>>});
        false ->
            convert(Rest, State#{currentToken := <<CT/binary, "#">>,
                                 rest         := <<>>})
    end;

%% Header end by end of data
convert(<<>>, #{state        := Tag,
                currentToken := CT,
                lastLine     := <<>>,
                html         := Html,
                rest         := Rest})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, BinTag/binary, Rest/binary>>;
convert(<<>>, #{state        := Tag,
                currentToken := CT,
                lastLine     := LL,
                html         := Html,
                rest         := Rest})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, LL/binary, ?brTag, BinTag/binary, Rest/binary>>;

%% Header end by #...
convert(<<" #", Rest/binary>>, State = #{state        := Tag,
                                         currentToken := CT,
                                         html         := Html})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    case headerEnd(Rest) of
        {true, Rest2} ->
            convert(<<Rest2/binary>>, State#{state        := Tag,
                                             currentToken := CT,
                                             html         := Html});
        false ->
            convert(Rest, State#{currentToken := <<CT/binary, " #">>,
                                 rest         := <<>>})
    end;

%% Header end by line end.
convert(<<13, 10, Rest/binary>>, State = #{state        := Tag,
                                           currentToken := CT,
                                           html         := Html})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    {LineEnding, Rest2} = getLineEnding(Rest),
    BinTag = makeTag(Tag, CT),
    Html2  = <<Html/binary, BinTag/binary, LineEnding/binary>>,
    convert(Rest2, State#{state        := text,
                          currentToken := <<>>,
                          html         := Html2,
                          rest         := <<>>});

%% Parse header text
convert(<<Char:8, Rest/binary>>, State = #{state := Tag, currentToken := CT})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    convert(Rest, State#{currentToken := <<CT/binary, Char:8>>, rest := <<>>});

%% Text line ending
convert(<<13, 10, Rest/binary>>, State = #{state        := text,
                                           currentToken := CT,
                                           lastLine     := <<>>}) ->
            {LineEnding, Rest2} = getLineEnding(CT, Rest),
            convert(Rest2, State#{lastLine     := <<CT/binary>>,
                                  currentToken := <<>>,
                                  rest         := LineEnding});
convert(<<13, 10, Rest/binary>>, State = #{state        := text,
                                           currentToken := CT,
                                           html         := Html,
                                           lastLine     := LL}) ->
    case header(CT, LL) of
        {true, Tag, Hdr} ->
            BinTag = makeTag(Tag, Hdr),
            Html2  = <<Html/binary, BinTag/binary, 13, 10>>,
            convert(Rest, State#{html         := Html2,
                                 lastLine     := <<>>,
                                 currentToken := <<>>,
                                 rest         := <<>>});
        false ->
            {LineEnding, Rest2} = getLineEnding(CT, Rest),
            Html2 = <<Html/binary, LL/binary, LineEnding/binary>>,
            convert(Rest2, State#{html         := Html2,
                                  lastLine     := CT,
                                  currentToken := <<>>,
                                  rest         := <<>>})
    end;

%% Parse text
convert(<<Char:8, Rest/binary>>, State = #{state        := text,
                                           currentToken := CT}) ->
    convert(Rest, State#{currentToken := <<CT/binary, Char:8>>, rest := <<>>});

%% Text end by end of data
convert(<<>>, #{state        := text,
                currentToken := CT,
                lastLine     := <<>>,
                html         := Html,
                rest         := Rest}) ->
    <<Html/binary, CT/binary, Rest/binary>>;
convert(<<>>, #{state        := text,
                currentToken := CT,
                lastLine     := LL,
                html         := Html,
                rest         := Rest}) ->
    case header(CT, LL) of
        {true, Tag, Hdr} ->
            BinTag = makeTag(Tag, Hdr),
            <<Html/binary, BinTag/binary, Rest/binary>>;
        false ->
            <<Html/binary, LL/binary, ?brTag, CT/binary, Rest/binary>>
    end.

makeTag(Tag, Content) ->
    BinTag = list_to_binary(atom_to_list(Tag)),
    <<$<, BinTag/binary, $>, Content/binary, $<, $/, BinTag/binary, $>>>.

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
headerEnd(<<$#, Rest/binary>>, start) ->
    headerEnd(Rest, start);
headerEnd(<<32, Rest/binary>>, start) ->
    headerEnd(Rest, space);
headerEnd(<<32, Rest/binary>>, space) ->
    headerEnd(Rest, space);
headerEnd(_, space) ->
    false.

header(CT, LL) ->
    case underline(CT) of
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

underline(Underline) ->
    underline(Underline, start).

underline(<<$=, Rest/binary>>, start) ->
    underline(Rest, h1, underline);
underline(<<$-, Rest/binary>>, start) ->
    underline(Rest, h2, underline);
underline(_CT, _) ->
    false.

underline(<<$=, Rest/binary>>, h1, underline) ->
    underline(Rest, h1, underline);
underline(<<$-, Rest/binary>>, h2, underline) ->
    underline(Rest, h2, underline);
underline(<<>>, Tag, underline) ->
    {true, Tag};
underline(_, _, _) ->
    false.

%% If current line ends with atleast two spaces, insert <br />.
getLineEnding(CT, Rest) when binary_part(CT, {byte_size(CT), -2}) == <<"  ">> ->
    {_, Rest2} = getLineEnding(Rest),
    {<<?brTag>>, Rest2};
getLineEnding(_CT, LineEnding) ->
    getLineEnding(LineEnding).

getLineEnding(<<13, 10, Rest/binary>>) -> {<<?brTag>>, Rest};
getLineEnding(<<10,     Rest/binary>>) -> {<<?brTag>>, Rest};
getLineEnding(LineEnding)              -> {<<13, 10>>, LineEnding}.
