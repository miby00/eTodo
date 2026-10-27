-module(eTodoMist).

-behaviour(gen_server).

-export([start_link/0]).
-export([read_bootstrap_stylesheet/0]).
-export([read_etodo_stylesheet/0]).
-export([read_etodo_script/0]).
-export([read_etodo_logo/0, read_etodo_icon/0]).
-export([read_linked_file/2]).
-export([status_json/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {server}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case application:get_env(eTodo, mistWebEnabled, false) of
        false ->
            ignore;
        true ->
            Port = application:get_env(eTodo, mistWebPort, 9003),
            Host = application:get_env(eTodo, mistWebBind, "127.0.0.1"),
            Cert = application:get_env(
                     eTodo, mistWebCert, "server_cert.pem.fullchain.pem"),
            Key = application:get_env(eTodo, mistWebKey, "server_key.pem"),
            start_server(Port, Host, Cert, Key)
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Server, Reason}, State = #state{server = Server}) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{server = Server}) ->
    exit(Server, shutdown),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

start_server(Port, Host, Cert, Key)
  when is_integer(Port), Port > 0, Port < 65536,
       is_list(Host), is_list(Cert), is_list(Key) ->
    PrivDir = code:priv_dir(eTodo),
    _ = eTodoCerts:ensure(PrivDir),
    CertFile = case resolve_tls_file(PrivDir, Cert) of
                   Missing when Cert =:= "server_cert.pem.fullchain.pem" ->
                       case filelib:is_file(Missing) of
                           true  -> Missing;
                           false -> resolve_tls_file(PrivDir, "server_cert.pem")
                       end;
                   Found -> Found
               end,
    KeyFile = resolve_tls_file(PrivDir, Key),
    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {true, true} ->
            Bind = list_to_binary(Host),
            CertBin = list_to_binary(CertFile),
            KeyBin = list_to_binary(KeyFile),
            Server = spawn_link(
                       fun() ->
                               etodo_web:start(Port, Bind, CertBin, KeyBin)
                       end),
            {ok, #state{server = Server}};
        {false, _} ->
            {stop, {missing_mist_web_certificate, CertFile}};
        {_, false} ->
            {stop, {missing_mist_web_private_key, KeyFile}}
    end;
start_server(Port, Host, Cert, Key) ->
    {stop, {invalid_mist_web_config, Port, Host, Cert, Key}}.

read_bootstrap_stylesheet() ->
    read_stylesheet("bootstrap.min.css").

read_etodo_stylesheet() ->
    read_stylesheet("etodo.css").

read_etodo_script() ->
    read_asset(filename:join(
                 [code:priv_dir(eTodo), "www", "priv", "js", "etodo.js"])).

read_etodo_icon() ->
    read_asset(filename:join(
                 [code:priv_dir(eTodo), "www", "priv", "Icons",
                  "etodoSuper.png"])).

read_etodo_logo() ->
    Logo = filename:join(
             [code:priv_dir(eTodo), "www", "priv", "Icons", "ETodo-300.png"]),
    read_asset(Logo).

%% The status page always shows the status of the owner of this installation.
status_json(_SessionUser) ->
    {ok, Owner} = application:get_env(eTodo, user),
    eTodoStatus:status_json(Owner).

%% Serves a file linked from the desktop client. The unguessable reference
%% is the only credential, as for the links of the old web client.
read_linked_file(Reference, FileName) ->
    case valid_reference(Reference) andalso valid_file_name(FileName) of
        true ->
            Path = filename:join([eTodoUtils:getUserCfgDir(), "linkedFiles",
                                  unicode:characters_to_list(Reference) ++ "_" ++
                                      unicode:characters_to_list(FileName)]),
            case file:read_file(Path) of
                {ok, Zipped} ->
                    unzip_linked_file(Zipped, FileName);
                {error, _} ->
                    {error, <<"not_found">>}
            end;
        false ->
            {error, <<"not_found">>}
    end.

unzip_linked_file(Zipped, FileName) ->
    try zlib:gunzip(Zipped) of
        Data ->
            Name = unicode:characters_to_list(FileName),
            MimeType = linked_mime_type(Name),
            {ok, {Data, unicode:characters_to_binary(MimeType),
                  unicode:characters_to_binary(disposition(MimeType, Name))}}
    catch
        _:_ -> {error, <<"not_found">>}
    end.

linked_mime_type(Name) ->
    case filename:extension(Name) of
        "" -> "application/octet-stream";
        _ -> eTodoUtils:mime_type(Name)
    end.

disposition(MimeType, _Name) when MimeType =:= "image/jpeg";
                                  MimeType =:= "image/png";
                                  MimeType =:= "image/gif";
                                  MimeType =:= "image/bmp" ->
    "inline";
disposition(_MimeType, Name) ->
    "attachment; filename*=UTF-8''" ++ uri_string:quote(Name).

valid_reference(Reference) when is_binary(Reference) ->
    Size = byte_size(Reference),
    Size >= 8 andalso Size =< 64 andalso
        lists:all(fun(C) -> (C >= $0 andalso C =< $9) orelse
                                (C >= $a andalso C =< $f) end,
                  binary_to_list(Reference));
valid_reference(_) ->
    false.

valid_file_name(FileName) when is_binary(FileName) ->
    case unicode:characters_to_list(FileName) of
        Name when is_list(Name), Name =/= [], length(Name) =< 255 ->
            Name =:= filename:basename(Name) andalso
                not lists:any(fun(C) -> C < 32 orelse C =:= $/ orelse
                                            C =:= $\\ orelse C =:= 127 end,
                              Name) andalso
                hd(Name) =/= $.;
        _ ->
            false
    end;
valid_file_name(_) ->
    false.

read_stylesheet(Name) ->
    Stylesheet = filename:join(
                   [code:priv_dir(eTodo), "www", "priv", "css",
                    Name]),
    read_asset(Stylesheet).

read_asset(Path) ->
    case file:read_file(Path) of
        {ok, Contents} ->
            {ok, Contents};
        {error, Reason} ->
            logger:error("Could not read Mist web asset ~p: ~p",
                         [Path, Reason]),
            {error, unicode:characters_to_binary(
                      io_lib:format("~tp", [Reason]))}
    end.

resolve_tls_file(PrivDir, File) ->
    case filename:pathtype(File) of
        absolute -> File;
        _ -> filename:join(PrivDir, File)
    end.
