-module(eTodoPush).

%% Pushes change notifications to web GUI Server-Sent Event streams.
%% Each stream is a Mist actor that subscribes with its Gleam subject.

-behaviour(gen_server).

-export([start_link/0, subscribe/2, notify/2, broadcast/1,
         messages_changed/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Proxies drop idle connections; a ping also detects closed streams.
-define(PING_INTERVAL, 25000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Called from the stream actor itself, so self() is the subscriber.
subscribe(User, Subject) ->
    gen_server:cast(?MODULE, {subscribe, self(), to_list(User), Subject}),
    nil.

notify(User, Event) ->
    gen_server:cast(?MODULE, {notify, to_list(User), Event}).

broadcast(Event) ->
    gen_server:cast(?MODULE, {broadcast, Event}).

messages_changed(User, msgEntry)    -> notify(User, <<"chat">>);
messages_changed(User, alarmEntry)  -> notify(User, <<"reminders">>);
messages_changed(User, systemEntry) -> notify(User, <<"system">>);
messages_changed(User, _Type)       -> notify(User, <<"messages">>).

init([]) ->
    erlang:send_after(?PING_INTERVAL, self(), ping),
    {ok, #{}}.

handle_call(_Request, _From, Subscribers) ->
    {reply, {error, unknown}, Subscribers}.

handle_cast({subscribe, Pid, User, Subject}, Subscribers) ->
    erlang:monitor(process, Pid),
    {noreply, Subscribers#{Pid => {User, Subject}}};
handle_cast({notify, User, Event}, Subscribers) ->
    [send(Subject, Event) || {Owner, Subject} <- maps:values(Subscribers),
                             Owner =:= User],
    {noreply, Subscribers};
handle_cast({broadcast, Event}, Subscribers) ->
    [send(Subject, Event) || {_User, Subject} <- maps:values(Subscribers)],
    {noreply, Subscribers}.

handle_info(ping, Subscribers) ->
    [send(Subject, <<"ping">>) || {_User, Subject} <- maps:values(Subscribers)],
    erlang:send_after(?PING_INTERVAL, self(), ping),
    {noreply, Subscribers};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, Subscribers) ->
    {noreply, maps:remove(Pid, Subscribers)};
handle_info(_Info, Subscribers) ->
    {noreply, Subscribers}.

terminate(_Reason, _Subscribers) ->
    ok.

code_change(_OldVersion, Subscribers, _Extra) ->
    {ok, Subscribers}.

send(Subject, Event) ->
    'gleam@erlang@process':send(Subject, Event).

to_list(Value) when is_binary(Value) -> unicode:characters_to_list(Value);
to_list(Value) when is_list(Value)   -> Value.
