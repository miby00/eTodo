#!/usr/bin/env escript
%%! -noshell
%%% Converts a sys.config to etodo.toml, using the setting names of
%%% eTodoConfig. Run it from the project folder after `make release`:
%%%
%%%   escript priv/sys_config_to_toml.escript config/sys.config > config/etodo.toml
%%%
%%% Add --env to write secrets as "%NAME%" placeholders, read from the
%%% environment at start-up, instead of copying them into the file.
%%% Settings without a TOML name are listed on stderr and not converted.

main(Args) ->
    io:setopts([{encoding, unicode}]),
    {Env, Files} = lists:partition(fun(A) -> A =:= "--env" end, Args),
    case Files of
        [File] -> convert(File, Env =/= []);
        _ ->
            io:format(standard_error,
                      "usage: sys_config_to_toml.escript [--env] sys.config~n",
                      []),
            halt(1)
    end.

convert(File, UseEnv) ->
    Root = filename:dirname(filename:dirname(filename:absname(
                                               escript:script_name()))),
    code:add_paths(filelib:wildcard(
                     filename:join([Root, "_build", "default", "lib", "*",
                                    "ebin"]))),
    case code:ensure_loaded(eTodoConfig) of
        {module, _} -> ok;
        _ ->
            fail("eTodoConfig not found, run `make release` first")
    end,
    Config = case file:consult(File) of
                 {ok, [Terms]} when is_list(Terms) -> Terms;
                 {error, Reason} ->
                     fail(io_lib:format("~ts: ~ts",
                                        [File, file:format_error(Reason)]))
             end,
    Settings = eTodoConfig:settings(),
    Used = [{Table, Key, Type, Value}
            || {Table, Key, App, Setting, Type} <- Settings,
               Value <- lookup(Config, App, Setting),
               Value =/= ""],
    reportSkipped(Config, Settings),
    Tables = lists:usort([T || {T, _, _, _} <- Used]),
    Order = lists:usort(fun(A, B) -> index(A, Settings) =< index(B, Settings)
                        end, Tables),
    io:format("# Converted from ~ts. See config/etodo.toml.example.~n",
              [File]),
    [writeTable(Table, [U || U = {T, _, _, _} <- Used, T =:= Table], UseEnv)
     || Table <- Order],
    ok.

lookup(Config, App, Setting) ->
    case lists:keyfind(App, 1, Config) of
        {App, Values} ->
            case lists:keyfind(Setting, 1, Values) of
                {Setting, Value} -> [Value];
                false -> []
            end;
        false -> []
    end.

index(Table, Settings) ->
    length(lists:takewhile(fun({T, _, _, _, _}) -> T =/= Table end,
                           Settings)).

reportSkipped(Config, Settings) ->
    Known = [{App, Setting} || {_, _, App, Setting, _} <- Settings],
    [io:format(standard_error, "not converted: {~p, ~p}~n", [App, Key])
     || {App, Values} <- Config, is_list(Values),
        {Key, _} <- Values, not lists:member({App, Key}, Known)].

writeTable(Table, Values, UseEnv) ->
    {Users, Plain} = lists:partition(fun({_, _, T, _}) -> T =:= users end,
                                     Values),
    case Plain of
        [] -> ok;
        _  ->
            io:format("~n[~ts]~n", [Table]),
            [io:format("~ts = ~ts~n", [Key, value(Type, Table, Key, V, UseEnv)])
             || {_, Key, Type, V} <- Plain]
    end,
    [begin
         io:format("~n[~ts.~ts]~n", [Table, Key]),
         [io:format("~ts = ~ts~n", [quote(Email), quote(User)])
          || {Email, User} <- V]
     end || {_, Key, users, V} <- Users],
    ok.

value(string, Table, Key, V, true) ->
    case secret(Key) of
        true  -> quote("%" ++ envName(Table, Key) ++ "%");
        false -> quote(V)
    end;
value(string, _Table, _Key, V, false) -> quote(V);
value(integer, _, _, V, _) -> integer_to_list(V);
value(boolean, _, _, V, _) -> atom_to_list(V);
value(Type, _, _, V, _) when Type =:= strings; Type =:= emails ->
    case io_lib:printable_unicode_list(V) of
        true  -> quote(V);
        false -> ["[", lists:join(", ", [quote(S) || S <- V]), "]"]
    end;
value(proxy, _, _, false, _) -> "false";
value(proxy, _, _, {Host, Port}, _) ->
    quote(Host ++ ":" ++ integer_to_list(Port)).

secret(Key) ->
    lists:member(Key, ["password", "client_secret", "token"]).

envName("node", "password") -> "ETODO_PASSWORD";
envName(Table, Key) -> string:uppercase(Table ++ "_" ++ Key).

quote(Text) ->
    Escaped = lists:flatmap(fun($\\) -> "\\\\";
                               ($") -> "\\\"";
                               ($\n) -> "\\n";
                               (C) -> [C]
                            end, Text),
    [$", Escaped, $"].

fail(Message) ->
    io:format(standard_error, "~ts~n", [Message]),
    halt(1).
