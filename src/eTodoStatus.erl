%%% Keeps the current status and countdown timer of a user so that the
%%% web UI can show them. Fed by the desktop client and by noGui mode.
-module(eTodoStatus).

-include_lib("eTodo/include/eTodo.hrl").

-export([set_status/3, set_timer/3, clear_timer/1, status_json/1,
         change_status/3]).

set_status(User, Status, StatusMsg) ->
    Info = get_info(User),
    put_info(User, Info#{status => Status, status_msg => StatusMsg}),
    eTodoPush:broadcast(<<"status">>).

set_timer(User, TimerRef, Description) ->
    Info = get_info(User),
    put_info(User, Info#{timer => TimerRef, timer_desc => Description}).

clear_timer(User) ->
    Info = get_info(User),
    put_info(User, Info#{timer => undefined, timer_desc => ""}).

%% Change status from the web UI: updates the desktop client when there is
%% one, otherwise the headless node, and tells the peers.
change_status(User, Status, StatusMsg) ->
    set_status(User, Status, StatusMsg),
    case application:get_env(eTodo, mode) of
        {ok, "noGui"} ->
            application:set_env(eTodo, status, Status),
            application:set_env(eTodo, statusMsg, StatusMsg),
            StatusUpdate = #userStatus{userName  = User,
                                       status    = Status,
                                       statusMsg = StatusMsg},
            Peers = [Peer || Peer <- eTodoDB:getUsers(), is_list(Peer),
                             Peer =/= User],
            ePeerEM:sendMsg(User, Peers, statusEntry,
                            {statusUpdate, StatusUpdate,
                             eGuiFunctions:getPortrait(User)});
        _ ->
            eTodo:webStatus(Status, StatusMsg)
    end,
    ok.

status_json(User) ->
    #{status := Status, status_msg := StatusMsg,
      timer := Timer, timer_desc := Desc} = get_info(User),
    Seconds = seconds_left(Timer),
    Json = json:encode(#{<<"user">>       => to_bin(User),
                         <<"status">>     => to_bin(Status),
                         <<"status_msg">> => to_bin(StatusMsg),
                         <<"timer">>      => Seconds,
                         <<"timer_desc">> => case Seconds of
                                                 0 -> <<>>;
                                                 _ -> to_bin(Desc)
                                             end}),
    {ok, iolist_to_binary(Json)}.

get_info(User) ->
    Default = #{status     => env(status, "Available"),
                status_msg => env(statusMsg, ""),
                timer      => undefined,
                timer_desc => ""},
    maps:merge(Default, persistent_term:get({?MODULE, User}, #{})).

put_info(User, Info) ->
    persistent_term:put({?MODULE, User}, Info).

env(Key, Default) ->
    case application:get_env(eTodo, Key) of
        {ok, Value} when is_list(Value) -> Value;
        _ -> Default
    end.

seconds_left(undefined) ->
    0;
seconds_left(Ref) ->
    case erlang:read_timer(Ref) of
        false -> 0;
        MSecs -> (MSecs + 999) div 1000
    end.

to_bin(Value) ->
    case unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end.
