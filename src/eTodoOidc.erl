%%%-------------------------------------------------------------------
%%% @doc
%%% Sign in to the web GUI with a Google account (OpenID Connect,
%%% authorization code flow). Google accounts are mapped to eTodo users
%%% in sys.config:
%%%
%%%   {mistGoogleClientId,     "....apps.googleusercontent.com"},
%%%   {mistGoogleClientSecret, "..."},
%%%   {mistGoogleRedirectUri,  "https://host:9003/auth/google/callback"},
%%%   {mistGoogleUsers, [{"someone@gmail.com", "Ann"}]}
%%%
%%% Only addresses listed in mistGoogleUsers can sign in.
%%%
%%% No server-side state is kept. The browser gets a short-lived cookie
%%% with a random value; the "state" and "nonce" sent to Google are HMACs
%%% of that value under a key that only exists in this VM.
%%% @end
%%%-------------------------------------------------------------------
-module(eTodoOidc).

-export([init/0, enabled/0, start/0, finish/3]).

-define(AUTH_URL,  "https://accounts.google.com/o/oauth2/v2/auth").
-define(TOKEN_URL, "https://oauth2.googleapis.com/token").
-define(ISSUERS,   [<<"https://accounts.google.com">>,
                    <<"accounts.google.com">>]).
-define(MAX_AGE,   600).

init() ->
    case persistent_term:get({?MODULE, key}, undefined) of
        undefined -> persistent_term:put({?MODULE, key},
                                         crypto:strong_rand_bytes(32));
        _         -> ok
    end,
    ok.

enabled() ->
    is_list(env(mistGoogleClientId)) andalso
        is_list(env(mistGoogleClientSecret)) andalso
        is_list(env(mistGoogleRedirectUri)) andalso
        env(mistGoogleUsers, []) =/= [].

%% Returns {RedirectUrl, CookieValue}.
start() ->
    Cookie = iolist_to_binary(
               [integer_to_binary(erlang:system_time(second)), ".",
                binary:encode_hex(crypto:strong_rand_bytes(16))]),
    Query = uri_string:compose_query(
              [{"client_id",     env(mistGoogleClientId)},
               {"redirect_uri",  env(mistGoogleRedirectUri)},
               {"response_type", "code"},
               {"scope",         "openid email"},
               {"state",         state(Cookie)},
               {"nonce",         nonce(Cookie)},
               {"prompt",        "select_account"}]),
    {iolist_to_binary([?AUTH_URL, "?", Query]), Cookie}.

%% Returns {ok, SessionToken} | {error, Message}.
finish(Code, State, Cookie) ->
    try
        ok = check_cookie(Cookie, State),
        IdToken = exchange_code(Code),
        Claims = check_claims(IdToken, Cookie),
        Email = string:lowercase(binary_to_list(maps:get(<<"email">>, Claims))),
        case lookup_user(Email) of
            {ok, User} ->
                case eTodoMistAuth:external_login(User) of
                    {ok, Token} ->
                        logger:notice("Google login: ~s -> ~s", [Email, User]),
                        {ok, Token};
                    {error, _} ->
                        fail({unknown_etodo_user, User})
                end;
            error ->
                logger:warning("Google login refused for ~s", [Email]),
                {error, <<"This Google account is not allowed to sign in.">>}
        end
    catch
        throw:{oidc_failed, Reason} ->
            logger:warning("Google login failed: ~p", [Reason]),
            {error, <<"Google sign-in failed. Try again.">>};
        Class:Reason2 ->
            logger:warning("Google login failed: ~p:~p", [Class, Reason2]),
            {error, <<"Google sign-in failed. Try again.">>}
    end.

check_cookie(Cookie, State) when is_binary(Cookie), is_binary(State) ->
    case binary:split(Cookie, <<".">>) of
        [TsBin, _] ->
            Ts = binary_to_integer(TsBin),
            Age = erlang:system_time(second) - Ts,
            case Age >= 0 andalso Age =< ?MAX_AGE andalso
                 same(state(Cookie), State) of
                true  -> ok;
                false -> fail(bad_state)
            end;
        _ ->
            fail(bad_cookie)
    end;
check_cookie(_, _) ->
    fail(missing_state).

exchange_code(Code) ->
    Body = uri_string:compose_query(
             [{"code",          Code},
              {"client_id",     env(mistGoogleClientId)},
              {"client_secret", env(mistGoogleClientSecret)},
              {"redirect_uri",  env(mistGoogleRedirectUri)},
              {"grant_type",    "authorization_code"}]),
    Url = env(mistGoogleTokenUrl, ?TOKEN_URL),
    SslOpts = [{verify, verify_peer},
               {cacerts, public_key:cacerts_get()},
               {customize_hostname_check,
                [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}],
    _ = application:ensure_all_started(inets),
    Profile = http_profile(),
    case httpc:request(post,
                       {Url, [], "application/x-www-form-urlencoded", Body},
                       [{ssl, SslOpts}, {timeout, 10000}],
                       [{body_format, binary}], Profile) of
        {ok, {{_, 200, _}, _, Resp}} ->
            case jsx:decode(Resp, [return_maps]) of
                #{<<"id_token">> := IdToken} -> IdToken;
                _ -> fail(no_id_token)
            end;
        {ok, {{_, Status, _}, _, _}} ->
            fail({token_endpoint_status, Status});
        {error, Reason} ->
            fail({token_endpoint_error, Reason})
    end.

http_profile() ->
    {Profile, Options} = case google_proxy() of
                             {undefined, []} -> {eTodoOidcDirect, []};
                             Proxy -> {?MODULE, [{https_proxy, Proxy}]}
                         end,
    case inets:start(httpc, [{profile, Profile}]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        {error, _Reason} -> fail(http_profile_start_failed)
    end,
    ok = httpc:set_options(Options, Profile),
    Profile.

google_proxy() ->
    case env(mistGoogleProxy) of
        undefined -> environment_proxy();
        false -> {undefined, []};
        {Host, Port} when is_list(Host), is_integer(Port),
                          Port > 0, Port =< 65535 ->
            {{Host, Port}, ["localhost", "127.0.0.1", "::1"]};
        _ -> fail(invalid_google_proxy)
    end.

environment_proxy() ->
    case os:getenv("https_proxy") of
        false -> proxy_url(os:getenv("HTTPS_PROXY"));
        "" -> proxy_url(os:getenv("HTTPS_PROXY"));
        Url -> proxy_url(Url)
    end.

proxy_url(false) -> {undefined, []};
proxy_url("") -> {undefined, []};
proxy_url(Url) ->
    case uri_string:parse(Url) of
        #{scheme := "http", host := Host} = Parsed when Host =/= "" ->
            Port = maps:get(port, Parsed, 80),
            case {maps:is_key(userinfo, Parsed), Port} of
                {false, Port} when is_integer(Port),
                                   Port > 0, Port =< 65535 ->
                    {{Host, Port}, ["localhost", "127.0.0.1", "::1"]};
                _ -> fail(invalid_google_proxy)
            end;
        _ -> fail(invalid_google_proxy)
    end.

%% The token comes straight from Google's token endpoint over verified TLS,
%% so its signature does not need checking (OpenID Connect Core 3.1.3.7).
check_claims(IdToken, Cookie) ->
    [_, Payload | _] = binary:split(IdToken, <<".">>, [global]),
    Claims = jsx:decode(base64:decode(Payload, #{mode => urlsafe,
                                                 padding => false}),
                        [return_maps]),
    Now = erlang:system_time(second),
    ClientId = list_to_binary(env(mistGoogleClientId)),
    Nonce = nonce(Cookie),
    Checks =
        [{issuer,   lists:member(maps:get(<<"iss">>, Claims, <<>>), ?ISSUERS)},
         {audience, maps:get(<<"aud">>, Claims, <<>>) =:= ClientId},
         {expired,  is_integer(maps:get(<<"exp">>, Claims, 0))
                        andalso maps:get(<<"exp">>, Claims, 0) > Now},
         {nonce,    maps:get(<<"nonce">>, Claims, <<>>) =:= Nonce},
         {email,    is_binary(maps:get(<<"email">>, Claims, undefined))},
         {verified, lists:member(maps:get(<<"email_verified">>, Claims, false),
                                 [true, <<"true">>])}],
    case [Name || {Name, false} <- Checks] of
        []     -> Claims;
        Failed -> fail({bad_claims, Failed})
    end.

lookup_user(Email) ->
    Users = [{string:lowercase(E), U} || {E, U} <- env(mistGoogleUsers, [])],
    case lists:keyfind(Email, 1, Users) of
        {_, User} -> {ok, User};
        false     -> error
    end.

same(A, B) ->
    byte_size(A) =:= byte_size(B) andalso crypto:hash_equals(A, B).

state(Cookie) -> mac(<<"state">>, Cookie).
nonce(Cookie) -> mac(<<"nonce">>, Cookie).

mac(Purpose, Cookie) ->
    Key = persistent_term:get({?MODULE, key}),
    binary:encode_hex(crypto:mac(hmac, sha256, Key, [Purpose, $:, Cookie]),
                      lowercase).

fail(Reason) ->
    throw({oidc_failed, Reason}).

env(Key) ->
    env(Key, undefined).

env(Key, Default) ->
    application:get_env(eTodo, Key, Default).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

proxy_url_test() ->
    ?assertEqual({undefined, []}, proxy_url(false)),
    ?assertEqual({undefined, []}, proxy_url("")),
    ?assertEqual({{"proxy.example", 8080},
                  ["localhost", "127.0.0.1", "::1"]},
                 proxy_url("http://proxy.example:8080")),
    ?assertEqual({{"proxy.example", 80},
                  ["localhost", "127.0.0.1", "::1"]},
                 proxy_url("http://proxy.example")),
    lists:foreach(
      fun(Url) ->
              ?assertThrow({oidc_failed, invalid_google_proxy}, proxy_url(Url))
      end,
      ["not a URL", "https://proxy.example", "http://proxy.example:0",
       "http://proxy.example:65536", "http://user:secret@proxy.example:8080"]).

profile_proxy_test() ->
    {ok, _} = application:ensure_all_started(inets),
    Previous = application:get_env(eTodo, mistGoogleProxy),
    {ok, Default} = httpc:get_options([https_proxy]),
    try
        application:set_env(eTodo, mistGoogleProxy, {"proxy.example", 8080}),
        ?assertEqual(?MODULE, http_profile()),
        ?assertEqual({ok, [{https_proxy,
                           {{"proxy.example", 8080},
                            ["localhost", "127.0.0.1", "::1"]}}]},
                     httpc:get_options([https_proxy], ?MODULE)),
        application:set_env(eTodo, mistGoogleProxy, false),
        ?assertEqual(eTodoOidcDirect, http_profile()),
        ?assertEqual({ok, [{https_proxy, {undefined, []}}]},
                 httpc:get_options([https_proxy], eTodoOidcDirect)),
        ?assertEqual({ok, Default}, httpc:get_options([https_proxy]))
    after
        inets:stop(httpc, ?MODULE),
        inets:stop(httpc, eTodoOidcDirect),
        case Previous of
            {ok, Value} -> application:set_env(eTodo, mistGoogleProxy, Value);
            undefined -> application:unset_env(eTodo, mistGoogleProxy)
        end
    end.
-endif.
