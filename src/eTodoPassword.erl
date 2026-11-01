%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @doc
%%% Salted PBKDF2-SHA256 hashes for the web password stored in mnesia.
%%% A hash is stored as {pbkdf2_sha256, Iterations, Salt, Hash}; a
%%% plain text password from an older version is still accepted by
%%% verify/2 until it has been hashed.
%%% @end
%%%-------------------------------------------------------------------
-module(eTodoPassword).
-author("mikael.bylund@gmail.com").

-export([hash/1, hash/2, verify/2, is_hash/1, protect/1]).

-define(ALG, pbkdf2_sha256).
-define(ITERATIONS, 600000).
-define(SALT_LEN, 16).
-define(KEY_LEN, 32).

hash(Password) ->
    hash(Password, ?ITERATIONS).

hash(Password, Iterations) ->
    Salt = crypto:strong_rand_bytes(?SALT_LEN),
    {?ALG, Iterations, Salt, derive(to_binary(Password), Salt, Iterations)}.

is_hash({?ALG, Iterations, Salt, Hash}) ->
    is_integer(Iterations) andalso Iterations > 0 andalso
        is_binary(Salt) andalso is_binary(Hash);
is_hash(_) ->
    false.

%% Turns a plain text password into a hash; empty means no password.
protect(undefined) ->
    undefined;
protect(Password) when Password =:= ""; Password =:= <<>> ->
    undefined;
protect(Stored) ->
    case is_hash(Stored) of
        true  -> Stored;
        false -> hash(Stored)
    end.

verify(Password, Stored) ->
    case to_binary(Password) of
        <<>> ->
            false;
        Bin ->
            case is_hash(Stored) of
                true ->
                    {?ALG, Iterations, Salt, Hash} = Stored,
                    crypto:hash_equals(derive(Bin, Salt, Iterations), Hash);
                false ->
                    verify_plain(Bin, Stored)
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

derive(Password, Salt, Iterations) ->
    crypto:pbkdf2_hmac(sha256, Password, Salt, Iterations, ?KEY_LEN).

verify_plain(Password, Stored) when is_list(Stored); is_binary(Stored) ->
    case to_binary(Stored) of
        <<>> ->
            false;
        StoredBin ->
            crypto:hash_equals(crypto:hash(sha256, Password),
                               crypto:hash(sha256, StoredBin))
    end;
verify_plain(_Password, _Stored) ->
    false.

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end;
to_binary(_) ->
    <<>>.
