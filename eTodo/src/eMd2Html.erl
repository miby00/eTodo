%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2017 19:34
%%%-------------------------------------------------------------------
-module(eMd2Html).
-author("miby00").

%% API
-export([convert/1]).

convert(MDText) when is_list(MDText) ->
    convert(list_to_binary(MDText));
convert(MDText) when is_binary(MDText) ->
    convert(MDText, #{state        => text,
                      currentToken => <<>>,
                      lastLine     => <<>>,
                      html         => <<>>}).

%% Headers
convert(<<"###### ", Rest/binary>>, State = #{state := text}) ->
    convert(Rest, State#{state := h6});
convert(<<"##### ", Rest/binary>>, State = #{state := text}) ->
    convert(Rest, State#{state := h5});
convert(<<"#### ", Rest/binary>>, State = #{state := text}) ->
    convert(Rest, State#{state := h4});
convert(<<"### ", Rest/binary>>, State = #{state := text}) ->
    convert(Rest, State#{state := h3});
convert(<<"## ", Rest/binary>>, State = #{state := text}) ->
    convert(Rest, State#{state := h2});
convert(<<"# ", Rest/binary>>, State = #{state := text}) ->
    convert(Rest, State#{state := h1});

convert(<<>>, #{state := Tag, currentToken := CT, lastLine := LL, html := Html})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    <<Html/binary, LL/binary, "<br />", BinTag/binary>>;

convert(<<" ######", 13, Rest/binary>>, State = #{state        := h6,
                                                  currentToken := CT,
                                                  html         := Html}) ->
    BinTag = makeTag(h6, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(<<13, Rest/binary>>, State#{state        := text,
                                        currentToken := <<>>,
                                        html         := Html2});
convert(<<" #####", 13, Rest/binary>>, State = #{state        := h5,
                                                 currentToken := CT,
                                                 html         := Html}) ->
    BinTag = makeTag(h5, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(<<13, Rest/binary>>, State#{state        := text,
                                        currentToken := <<>>,
                                        html         := Html2});
convert(<<" ####", 13, Rest/binary>>, State = #{state        := h4,
                                                currentToken := CT,
                                                html         := Html}) ->
    BinTag = makeTag(h4, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(<<13, Rest/binary>>, State#{state        := text,
                                        currentToken := <<>>,
                                        html         := Html2});
convert(<<" ###", 13, Rest/binary>>, State = #{state        := h3,
                                               currentToken := CT,
                                               html         := Html}) ->
    BinTag = makeTag(h3, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(<<13, Rest/binary>>, State#{state        := text,
                                        currentToken := <<>>,
                                        html         := Html2});
convert(<<" ##", 13, Rest/binary>>, State = #{state        := h2,
                                              currentToken := CT,
                                              html         := Html}) ->
    BinTag = makeTag(h2, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(<<13, Rest/binary>>, State#{state        := text,
                                        currentToken := <<>>,
                                        html         := Html2});
convert(<<" #", 13, Rest/binary>>, State = #{state        := h1,
                                             currentToken := CT,
                                             html         := Html}) ->
    BinTag = makeTag(h1, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(<<13, Rest/binary>>, State#{state        := text,
                                        currentToken := <<>>,
                                        html         := Html2});

convert(<<13, 10, Rest/binary>>, State = #{state        := Tag,
                                           currentToken := CT,
                                           html         := Html})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    BinTag = makeTag(Tag, CT),
    Html2  = <<Html/binary, BinTag/binary>>,
    convert(Rest, State#{state := text, currentToken := <<>>, html := Html2});

convert(<<Char:8, Rest/binary>>, State = #{state := Tag, currentToken := CT})
    when (Tag == h6) or (Tag == h5) or (Tag == h4) or
         (Tag == h3) or (Tag == h2) or (Tag == h1) ->
    convert(Rest, State#{currentToken := <<CT/binary, Char:8>>});

%% Text
convert(<<13, 10, Rest/binary>>, State = #{state        := text,
                                           currentToken := CT,
                                           lastLine     := <<>>}) ->
            convert(Rest, State#{lastLine     := <<CT/binary>>,
                                  currentToken := <<>>});
convert(<<13, 10, Rest/binary>>, State = #{state        := text,
                                           currentToken := CT,
                                           html         := Html,
                                           lastLine     := LL}) ->
    case h1(CT, LL) of
        true ->
            BinTag = makeTag(h1, LL),
            Html2  = <<Html/binary, BinTag/binary, "<br />">>,
            convert(Rest, State#{html          := Html2,
                                  lastLine     := <<>>,
                                  currentToken := <<>>});
        false ->
            Html2 = <<Html/binary, LL/binary, "<br />">>,
            convert(Rest, State#{html          := Html2,
                                  lastLine     := CT,
                                  currentToken := <<>>})
    end;
convert(<<Char:8, Rest/binary>>, State = #{state        := text,
                                           currentToken := CT}) ->
    convert(Rest, State#{currentToken := <<CT/binary, Char:8>>});
convert(<<>>, State = #{state        := text,
                        currentToken := CT,
                        lastLine     := LL,
                        html         := Html}) ->
    case h1(CT, LL) of
        true ->
            BinTag = makeTag(h1, LL),
            {<<Html/binary, BinTag/binary>>, State};
        false ->
            {<<Html/binary, LL/binary, "<br />", CT/binary>>, State}
    end.

makeTag(Tag, Content) ->
    BinTag = list_to_binary(atom_to_list(Tag)),
    <<"<", BinTag/binary, ">", Content/binary, "</", BinTag/binary, ">">>.

h1(CT, LL) when (size(CT) == size(LL)) ->
    underline(CT);
h1(_CT, _LL) ->
    false.

underline(<<$=, Rest/binary>>) ->
    underline(Rest, $=);
underline(<<$-, Rest/binary>>) ->
    underline(Rest, $-);
underline(_CT) ->
    false.

underline(<<$=, Rest/binary>>, $=) ->
    underline(Rest, $=);
underline(<<$-, Rest/binary>>, $-) ->
    underline(Rest, $-);
underline(<<>>, _) ->
    true;
underline(_, _) ->
    false.