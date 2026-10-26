%%% Blind TCP tunnel that lets the web UI be reached through another peer,
%%% for example from behind a firewall. The relay peer only copies the bytes
%%% of the browser's TLS connection to the web UI of the owner, so it cannot
%%% read or change anything.
%%%
%%% Relay peer (accepts browsers, forwards to the owner through the peers):
%%%   {webRelayPort, 9005}, {webRelayTarget, "OwnerUser"}
%%% Owner (accepts tunnels from the listed relay peers only):
%%%   {webTunnelPeers, ["RelayUser"]}
-module(eTodoTunnel).

-behaviour(gen_server).

-export([start_link/0, handle/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, eTodoTunnel).
-define(CHUNK, 16384).
-define(IDLE_TIMEOUT, 10 * 60 * 1000).
-define(MAX_CONNECTIONS, 50).

-record(state, {listener}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Tunnel message from a peer, delivered by eTodoEH.
%% Messages going up are sent by the relay to the owner, and messages going
%% down are the replies.
handle(Peer, {up, {open, Id}}) when is_binary(Id) ->
    gen_server:cast(?MODULE, {open, Peer, Id});
handle(Peer, {Dir, {data, Id, Data}})
  when is_binary(Id), is_binary(Data), (Dir =:= up orelse Dir =:= down) ->
    forward(Dir, Peer, Id, {data, Data});
handle(Peer, {Dir, {close, Id}})
  when is_binary(Id), (Dir =:= up orelse Dir =:= down) ->
    forward(Dir, Peer, Id, close);
handle(_Peer, _Message) ->
    ok.

forward(Dir, Peer, Id, Message) ->
    Role = case Dir of up -> owner; down -> relay end,
    case ets:lookup(?TABLE, {Role, Peer, Id}) of
        [{_, Pid}] -> Pid ! {tunnel, Message};
        [] -> ok
    end,
    ok.

init([]) ->
    ets:new(?TABLE, [named_table, public, set]),
    process_flag(trap_exit, true),
    case {application:get_env(eTodo, webRelayPort),
          application:get_env(eTodo, webRelayTarget)} of
        {{ok, Port}, {ok, Target}} when is_integer(Port), is_list(Target) ->
            listen(Port, Target);
        _ ->
            {ok, #state{}}
    end.

listen(Port, Target) ->
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true},
                               {packet, raw}]) of
        {ok, Listen} ->
            Self = self(),
            Pid = spawn_link(fun() -> accept_loop(Listen, Target, Self) end),
            {ok, #state{listener = {Listen, Pid}}};
        {error, Reason} ->
            eLog:log(error, ?MODULE, listen, [Port, Target],
                     "Could not start web relay: " ++
                         lists:flatten(io_lib:format("~p", [Reason])), ?LINE),
            {ok, #state{}}
    end.

accept_loop(Listen, Target, Server) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Id = binary:encode_hex(crypto:strong_rand_bytes(8)),
            Pid = spawn(fun() -> relay_start(Socket, Target, Id) end),
            gen_tcp:controlling_process(Socket, Pid),
            Pid ! go,
            accept_loop(Listen, Target, Server);
        {error, closed} ->
            ok;
        {error, _} ->
            timer:sleep(200),
            accept_loop(Listen, Target, Server)
    end.

relay_start(Socket, Target, Id) ->
    receive go -> ok end,
    case ets:info(?TABLE, size) < ?MAX_CONNECTIONS of
        true ->
            ets:insert(?TABLE, {{relay, Target, Id}, self()}),
            send(Target, up, {open, Id}),
            pump(Socket, relay, Target, Id);
        false ->
            gen_tcp:close(Socket)
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({open, Peer, Id}, State) ->
    Allowed = case application:get_env(eTodo, webTunnelPeers) of
                  {ok, Peers} when is_list(Peers) -> lists:member(Peer, Peers);
                  _ -> false
              end,
    case Allowed andalso ets:info(?TABLE, size) < ?MAX_CONNECTIONS of
        true ->
            Pid = spawn(fun() -> owner_start(Peer, Id) end),
            ets:insert(?TABLE, {{owner, Peer, Id}, Pid});
        false ->
            send(Peer, down, {close, Id})
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = {Listen, _Pid}}) ->
    gen_tcp:close(Listen),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% Owner side: connect to the local web UI.
owner_start(Peer, Id) ->
    Port = application:get_env(eTodo, mistWebPort, 9003),
    Host = case application:get_env(eTodo, mistWebBind, "127.0.0.1") of
               "0.0.0.0" -> "127.0.0.1";
               Bind -> Bind
           end,
    case gen_tcp:connect(Host, Port, [binary, {active, false},
                                      {packet, raw}], 5000) of
        {ok, Socket} ->
            pump(Socket, owner, Peer, Id);
        {error, _} ->
            send(Peer, down, {close, Id}),
            ets:delete(?TABLE, {owner, Peer, Id})
    end.

%% Copies bytes between the socket and the peer until either side closes.
%% The relay sends up and the owner sends down.
pump(Socket, Role, Peer, Id) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            send_data(Peer, dir(Role), Id, Data),
            pump(Socket, Role, Peer, Id);
        {tcp_closed, Socket} ->
            finish(Socket, Role, Peer, Id, true);
        {tcp_error, Socket, _Reason} ->
            finish(Socket, Role, Peer, Id, true);
        {tunnel, {data, Data}} ->
            case gen_tcp:send(Socket, Data) of
                ok -> pump(Socket, Role, Peer, Id);
                {error, _} -> finish(Socket, Role, Peer, Id, true)
            end;
        {tunnel, close} ->
            finish(Socket, Role, Peer, Id, false)
    after ?IDLE_TIMEOUT ->
            finish(Socket, Role, Peer, Id, true)
    end.

dir(relay) -> up;
dir(owner) -> down.

finish(Socket, Role, Peer, Id, Notify) ->
    ets:delete(?TABLE, {Role, Peer, Id}),
    gen_tcp:close(Socket),
    case Notify of
        true -> send(Peer, dir(Role), {close, Id});
        false -> ok
    end.

send_data(Peer, Dir, Id, Data) when byte_size(Data) =< ?CHUNK ->
    send(Peer, Dir, {data, Id, Data});
send_data(Peer, Dir, Id, Data) ->
    <<Chunk:?CHUNK/binary, Rest/binary>> = Data,
    send(Peer, Dir, {data, Id, Chunk}),
    send_data(Peer, Dir, Id, Rest).

send(Peer, Dir, Message) ->
    {ok, User} = application:get_env(eTodo, user),
    ePeerEM:sendMsg(User, [Peer], tunnel, {Dir, Message}).
