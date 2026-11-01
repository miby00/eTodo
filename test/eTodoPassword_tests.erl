-module(eTodoPassword_tests).

-include_lib("eunit/include/eunit.hrl").

hash_verify_test() ->
    Hash = eTodoPassword:hash("S3cret!", 1000),
    ?assert(eTodoPassword:is_hash(Hash)),
    ?assert(eTodoPassword:verify(<<"S3cret!">>, Hash)),
    ?assert(eTodoPassword:verify("S3cret!", Hash)),
    ?assertNot(eTodoPassword:verify(<<"s3cret!">>, Hash)),
    ?assertNot(eTodoPassword:verify(<<>>, Hash)).

hash_is_salted_test() ->
    ?assertNotEqual(eTodoPassword:hash("pw", 1000),
                    eTodoPassword:hash("pw", 1000)).

unicode_password_test() ->
    Hash = eTodoPassword:hash("lösenord€", 1000),
    ?assert(eTodoPassword:verify(<<"lösenord€"/utf8>>, Hash)).

default_iterations_test() ->
    {pbkdf2_sha256, Iterations, _Salt, _Hash} = eTodoPassword:hash("pw"),
    ?assert(Iterations >= 600000).

plain_text_legacy_test() ->
    ?assert(eTodoPassword:verify(<<"old">>, "old")),
    ?assertNot(eTodoPassword:verify(<<"other">>, "old")),
    ?assertNot(eTodoPassword:verify(<<"old">>, undefined)),
    ?assertNot(eTodoPassword:verify(<<"x">>, "")).

protect_test() ->
    ?assertEqual(undefined, eTodoPassword:protect(undefined)),
    ?assertEqual(undefined, eTodoPassword:protect("")),
    Hash = eTodoPassword:hash("pw", 1000),
    ?assertEqual(Hash, eTodoPassword:protect(Hash)),
    Protected = eTodoPassword:protect("pw"),
    ?assert(eTodoPassword:is_hash(Protected)),
    ?assert(eTodoPassword:verify("pw", Protected)).
