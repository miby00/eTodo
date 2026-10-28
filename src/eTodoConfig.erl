%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @doc
%%% Reads etodo.toml and applies it on top of sys.config. The first file
%%% found is used:
%%%   1. the file named by the environment variable ETODO_CONFIG
%%%   2. /etc/etodo/etodo.toml
%%%   3. ~/.eTodo/etodo.toml (%APPDATA%/eTodo/etodo.toml on Windows)
%%%   4. etc/etodo.toml in the release (copied from config/etodo.toml)
%%%
%%% A string written as "%NAME%" is replaced by the environment variable
%%% NAME, both in etodo.toml and in the eTodo part of sys.config.
%%% @end
%%%-------------------------------------------------------------------
-module(eTodoConfig).
-author("mikael.bylund@gmail.com").

-export([load/0, settings/0]).

%% {Table, Key, Application, Setting, Type}
settings() ->
    [{"storage", "database_dir", mnesia, dir, string},
     {"storage", "log_dir", eLog, logDir, string},

     {"node", "mode", eTodo, mode, string},
     {"node", "user", eTodo, user, string},
     {"node", "password", eTodo, pwd, string},
     {"node", "circle", eTodo, circle, string},
     {"node", "status", eTodo, status, string},
     {"node", "status_message", eTodo, statusMsg, string},

     {"web", "enabled", eTodo, mistWebEnabled, boolean},
     {"web", "port", eTodo, mistWebPort, integer},
     {"web", "bind", eTodo, mistWebBind, string},
     {"web", "certificate", eTodo, mistWebCert, string},
     {"web", "key", eTodo, mistWebKey, string},
     {"web", "link_host", eTodo, mistWebLinkHost, string},

     {"relay", "port", eTodo, webRelayPort, integer},
     {"relay", "target", eTodo, webRelayTarget, string},
     {"tunnel", "peers", eTodo, webTunnelPeers, strings},

     {"google", "client_id", eTodo, mistGoogleClientId, string},
     {"google", "client_secret", eTodo, mistGoogleClientSecret, string},
     {"google", "redirect_uri", eTodo, mistGoogleRedirectUri, string},
     {"google", "token_url", eTodo, mistGoogleTokenUrl, string},
     {"google", "proxy", eTodo, mistGoogleProxy, proxy},
     {"google", "users", eTodo, mistGoogleUsers, users},

     {"smtp", "enabled", eTodo, smtpEnabled, boolean},
     {"smtp", "server", eTodo, smtpServer, string},
     {"smtp", "port", eTodo, smtpPort, integer},
     {"smtp", "auth", eTodo, smtpAuth, string},
     {"smtp", "user", eTodo, smtpUser, string},
     {"smtp", "password", eTodo, smtpPwd, string},

     {"notification", "email", eTodo, notificationEmail, emails},
     {"notification", "time", eTodo, notificationTime, integer},

     {"slack", "url", eTodo, slackUrl, string},
     {"slack", "token", eTodo, slackToken, string},
     {"slack", "client_id", eTodo, slackClientId, string},
     {"slack", "client_secret", eTodo, slackClientSecret, string},

     {"jira", "url", eTodo, jiraUrl, string},
     {"jira", "user", eTodo, jiraUser, string},
     {"jira", "password", eTodo, jiraPwd, string},
     {"jira", "search", eTodo, jiraSearch, string}].

%% Returns ok or {error, Reason}; a broken file stops the start, so that
%% eTodo never runs with for example the wrong database folder.
load() ->
    resolveSysConfig(),
    case findFile() of
        none -> ok;
        File -> loadFile(File)
    end.

findFile() ->
    Candidates = [os:getenv("ETODO_CONFIG"),
                  "/etc/etodo/etodo.toml",
                  userFile(),
                  filename:join([code:root_dir(), "etc", "etodo.toml"])],
    case [File || File <- Candidates, is_list(File), File =/= "",
                  filelib:is_regular(File)] of
        [File | _] -> File;
        []         -> none
    end.

userFile() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("APPDATA") of
                false   -> false;
                AppData -> filename:join([AppData, "eTodo", "etodo.toml"])
            end;
        _ ->
            case init:get_argument(home) of
                {ok, [[Home]]} -> filename:join([Home, ".eTodo", "etodo.toml"]);
                _              -> false
            end
    end.

loadFile(File) ->
    case file:read_file(File) of
        {ok, Text} ->
            case etodo_config:parse(Text) of
                {ok, Config} ->
                    applyConfig(File, Config);
                {error, Reason} ->
                    configError(File, binary_to_list(Reason))
            end;
        {error, Reason} ->
            configError(File, file:format_error(Reason))
    end.

applyConfig(File, Config) ->
    try
        Values = convert(Config),
        Restart = [App || {App, Key, Value} <- Values,
                          setEnv(App, Key, Value)],
        [restart(App) || App <- lists:usort(Restart), App =/= eTodo],
        logger:notice("eTodo: using settings from ~ts", [File]),
        ok
    catch
        throw:{config, Reason} -> configError(File, Reason)
    end.

configError(File, Reason) ->
    Text = lists:flatten(io_lib:format("~ts: ~ts", [File, Reason])),
    logger:error("eTodo: bad configuration file ~ts", [Text]),
    io:format(standard_error, "eTodo: bad configuration file ~ts~n", [Text]),
    {error, {bad_config, Text}}.

%% Returns true when an already running application must pick up the value.
setEnv(App, Key, Value) ->
    Changed = application:get_env(App, Key) =/= {ok, Value},
    application:set_env(App, Key, Value, [{persistent, true}]),
    Changed andalso App =/= eTodo.

restart(App) ->
    case lists:keymember(App, 1, application:which_applications()) of
        true ->
            application:stop(App),
            ok = application:start(App);
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% TOML to application settings
%%--------------------------------------------------------------------
convert(Config) ->
    Settings = settings(),
    lists:append([table(Settings, Table, Values)
                  || {Table, Values} <- lists:sort(maps:to_list(Config))]).

table(Settings, Table, Values) when is_map(Values) ->
    Name = binary_to_list(Table),
    [setting(Settings, Name, binary_to_list(Key), Value)
     || {Key, Value} <- lists:sort(maps:to_list(Values)),
        known(Settings, Name, binary_to_list(Key))];
table(_Settings, Table, _Value) ->
    throw({config, io_lib:format("~ts must be a table, like [~ts]",
                                 [Table, Table])}).

known(Settings, Table, Key) ->
    case [S || S = {T, K, _, _, _} <- Settings, T =:= Table, K =:= Key] of
        [] ->
            logger:warning("eTodo: unknown setting ~ts.~ts is ignored",
                           [Table, Key]),
            false;
        _ ->
            true
    end.

setting(Settings, Table, Key, Value) ->
    [{App, Setting, Type}] = [{A, S, Ty} || {T, K, A, S, Ty} <- Settings,
                                            T =:= Table, K =:= Key],
    {App, Setting, value(Type, Value, Table ++ "." ++ Key)}.

value(string, Value, _Name) when is_binary(Value) ->
    resolve(text(Value));
value(integer, Value, _Name) when is_integer(Value) ->
    Value;
value(boolean, Value, _Name) when is_boolean(Value) ->
    Value;
value(emails, Value, Name) when is_binary(Value) ->
    [value(string, Value, Name)];
value(emails, Values, Name) when is_list(Values) ->
    value(strings, Values, Name);
value(strings, Values, Name) when is_list(Values) ->
    [value(string, Value, Name) || Value <- Values];
value(users, Users, Name) when is_map(Users) ->
    [user(text(Email), User, Name)
     || {Email, User} <- lists:sort(maps:to_list(Users))];
value(proxy, false, _Name) ->
    false;
value(proxy, Value, Name) when is_binary(Value) ->
    case string:split(text(Value), ":", trailing) of
        [Host, Port] when Host =/= "" ->
            try list_to_integer(Port) of
                Number -> {Host, Number}
            catch
                error:badarg -> badValue(proxy, Name)
            end;
        _ ->
            badValue(proxy, Name)
    end;
value(Type, _Value, Name) ->
    badValue(Type, Name).

%% A missing [table] header above other settings ends up here, so say so.
user(Email, User, Name) ->
    case lists:member($@, Email) of
        true ->
            {Email, value(string, User, Name ++ " \"" ++ Email ++ "\"")};
        false ->
            throw({config,
                   io_lib:format("~ts: ~ts is not an email address, is a "
                                 "[table] header missing or commented out "
                                 "above it?", [Name, Email])})
    end.

badValue(Type, Name) ->
    Expected = case Type of
                   string  -> "a text in quotes";
                   integer -> "a number";
                   boolean -> "true or false";
                   strings -> "a list of texts, like [\"a\", \"b\"]";
                   emails  -> "an address or a list of addresses";
                   users   -> "a table of \"email\" = \"user\"";
                   proxy   -> "\"host:port\" or false"
               end,
    throw({config, io_lib:format("~ts must be ~ts", [Name, Expected])}).

text(Binary) ->
    unicode:characters_to_list(Binary).

%%--------------------------------------------------------------------
%% "%NAME%" placeholders
%%--------------------------------------------------------------------
resolveSysConfig() ->
    [application:set_env(eTodo, Key, resolveTerm(Value))
     || {Key, Value} <- application:get_all_env(eTodo)],
    ok.

resolveTerm(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true  -> resolve(Value);
        false -> [resolveTerm(Item) || Item <- Value]
    end;
resolveTerm(Value) when is_tuple(Value) ->
    list_to_tuple(resolveTerm(tuple_to_list(Value)));
resolveTerm(Value) ->
    Value.

resolve([$% | Rest] = Value) when length(Rest) > 1 ->
    case lists:last(Rest) of
        $% -> lookupEnv(lists:droplast(Rest), Value);
        _  -> Value
    end;
resolve(Value) ->
    Value.

lookupEnv(Name, Original) ->
    case validName(Name) andalso os:getenv(Name) of
        false ->
            case validName(Name) of
                true ->
                    logger:warning("eTodo: environment variable ~ts is not "
                                   "set", [Name]);
                false ->
                    ok
            end,
            Original;
        Value ->
            Value
    end.

validName(Name) ->
    lists:all(fun(C) -> C =:= $_ orelse (C >= $A andalso C =< $Z) orelse
                            (C >= $a andalso C =< $z) orelse
                            (C >= $0 andalso C =< $9) end, Name).
