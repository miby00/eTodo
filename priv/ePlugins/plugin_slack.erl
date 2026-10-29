%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%% Slack integration for eTodo, using Slack's Socket Mode.
%%%
%%% Chat with Slack users and channels, keep the eTodo and Slack status
%%% in sync (Busy is Do Not Disturb, running timers are shown in the
%%% Slack status), create a task by reacting with :memo: on a message,
%%% post the work log and get eTodo reminders from Slackbot.
%%%
%%% Each eTodo user needs his/her own Slack app, Socket Mode spreads
%%% the events of one app over all its connections.
%%%
%%% 1) Go to https://api.slack.com/apps, choose "Create New App",
%%%    "From an app manifest" and paste slack_app_manifest.yml from
%%%    this directory.
%%% 2) Under "Basic Information", "App-Level Tokens", generate a token
%%%    with the connections:write scope (xapp-...).
%%% 3) Under "OAuth & Permissions", install the app in the workspace
%%%    and copy the "User OAuth Token" (xoxp-...).
%%% 4) Right click in the task list, choose "Slack settings..." and
%%%    enter both tokens. They can also be set as token and app_token
%%%    under [slack] in etodo.toml, where task_reaction selects another
%%%    reaction than :memo: for creating tasks.
%%% @end
%%% Created : 08 July 2013 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_slack).

-export([getName/0, getDesc/0, getMenu/2, init/1, terminate/2]).

-export([eGetStatusUpdate/5,
         eTimerStarted/7,
         eTimerStopped/3,
         eTimerEnded/4,
         eReceivedMsg/5,
         eReceivedSysMsg/3,
         eReceivedAlarmMsg/3,
         eLoggedInMsg/3,
         eLoggedOutMsg/3,
         eSendMsg/5,
         eSetStatusUpdate/5,
         eSetWorkLogDate/4,
         eMenuEvent/6,
         handleInfo/2]).

getName() -> "eSlack".

getDesc() -> "An eTodo Slack integration.".

-define(Timeout,      30000).
-define(DefaultUrl,   "https://slack.com/api").
-define(PresencePoll, 60000).
-define(MinBackoff,   1000).
-define(MaxBackoff,   60000).
-define(BusySnooze,   720).       %% Minutes of Do Not Disturb when Busy
-define(MaxFileSize,  20000000).  %% Bigger files are linked to Slack
-define(MaxStatus,    100).       %% Longest status text in Slack
-define(MaxTaskDesc,  120).

-define(log(Function, Args, Text),
        eLog:log(debug, ?MODULE, Function, Args, Text, ?LINE)).

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {frame,
                slackUrl,
                userToken     = "",
                appToken      = "",
                taskReaction  = <<"memo">>,
                timerStatus   = true,
                alarmsToSlack = false,
                myId,
                srvStatus     = "Available",
                srvStatusMsg  = "",
                status, statusText, presence,
                timer         = false,  %% A timer owns the Slack status
                profile,                %% Last users.profile.set params
                workLogDate,
                users     = #{},  %% UserId    => user
                convs     = #{},  %% ChannelId => conversation
                ims       = #{},  %% UserId    => direct message ChannelId
                portraits = #{},  %% UserId    => true, portrait fetched
                requests  = #{},  %% httpc RequestId => request
                reported  = #{},  %% {Method, Error} => true, shown once
                wsConn, wsPath, wsRef,
                backoff   = ?MinBackoff}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init([WX, Frame]) ->
    wx:set_env(WX),
    application:ensure_all_started(gun),
    Url = application:get_env(eTodo, slackUrl, ?DefaultUrl),
    self() ! slackStart,
    erlang:send_after(?PresencePoll, self(), slackPoll),
    loadConfig(#state{frame = Frame, slackUrl = Url}).

loadConfig(State) ->
    Cfg      = readConfig(),
    Reaction = application:get_env(eTodo, slackTaskReaction, "memo"),
    State#state{userToken     = token(userToken, slackToken,    Cfg),
                appToken      = token(appToken,  slackAppToken, Cfg),
                taskReaction  = unicode:characters_to_binary(
                                  string:trim(Reaction, both, ":")),
                timerStatus   = maps:get(timerStatus,   Cfg, true),
                alarmsToSlack = maps:get(alarmsToSlack, Cfg, false)}.

%% A token saved from the settings dialog wins over etodo.toml.
token(Key, EnvKey, Cfg) ->
    case maps:get(Key, Cfg, "") of
        ""    -> application:get_env(eTodo, EnvKey, "");
        Token -> Token
    end.

readConfig() ->
    try ePluginInterface:readConfig(?MODULE)
    catch _:_ -> #{}
    end.

saveSetting(Key, Value) ->
    ePluginInterface:saveConfig(?MODULE, (readConfig())#{Key => Value}).

%%--------------------------------------------------------------------
%% @doc
%% Free internal data for plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @doc
%% Return key value list of right menu options.
%% Menu option should be a unique integer bigger than 1300.
%% @spec getMenu(ETodo, State) -> {ok, [{menuOption, menuText}, ...], NewState}
%% @end
%%--------------------------------------------------------------------
getMenu(_ETodo, State) ->
    {ok, [{1550, "Add user(s)"},
          {1551, "Add channel(s)"},
          {1554, "Post work log to Slack..."},
          divider,
          {1555, onOff("Show timers in Slack status", State#state.timerStatus)},
          {1556, onOff("Send reminders to Slack", State#state.alarmsToSlack)},
          divider,
          {1552, "Slack settings..."},
          {1553, "Reconnect to Slack"}], State}.

onOff(Text, true)  -> Text ++ " (on)";
onOff(Text, false) -> Text ++ " (off)".

%%--------------------------------------------------------------------
%% @doc
%% Called every 5 seconds to check if someone changes status
%% outside eTodo.
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       {ok, Status, StatusMsg, NewState}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(_Dir, _User, Status, StatusMsg, State) ->
    NewStatus    = default(State#state.status, Status),
    NewStatusMsg = default(State#state.statusText, StatusMsg),
    {ok, NewStatus, NewStatusMsg,
     State#state{status       = undefined,
                 statusText   = undefined,
                 srvStatus    = NewStatus,
                 srvStatusMsg = NewStatusMsg}}.

default(undefined, Value) when is_binary(Value) ->
    unicode:characters_to_list(Value, utf8);
default(Value, _Default)  when is_binary(Value) ->
    unicode:characters_to_list(Value, utf8);
default(undefined, Value) -> Value;
default(Value, _Default)  -> Value.

%%--------------------------------------------------------------------
%% @doc
%% The user start timer
%%
%% @spec eTimerStarted(EScriptDir, User, Text, Hours, Min, Sec, State) ->
%%                     NewState
%% @end
%%--------------------------------------------------------------------
eTimerStarted(_Dir, _User, _Text, _Hours, _Min, _Sec,
              State = #state{userToken = ""}) ->
    State;
eTimerStarted(_Dir, _User, _Text, _Hours, _Min, _Sec,
              State = #state{timerStatus = false}) ->
    State;
eTimerStarted(_Dir, _User, Text, Hours, Min, Sec, State) ->
    Seconds = Hours * 3600 + Min * 60 + Sec,
    Profile = #{status_text       => timerText(Text),
                status_emoji      => <<":tomato:">>,
                status_expiration => erlang:system_time(second) + Seconds},
    State2  = setProfile(Profile, State#state{timer = true}),
    callApi("dnd.setSnooze", [{num_minutes, max(1, (Seconds + 59) div 60)}],
            none, State2).

timerText(Text) ->
    case statusText(Text) of
        <<>>  -> <<"Focus">>;
        Text2 -> Text2
    end.

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(EScriptDir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerStopped(_EScriptDir, _User, State) ->
    timerDone(State).

%%--------------------------------------------------------------------
%% @doc
%% Timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerEnded(_EScriptDir, _User, _Text, State) ->
    timerDone(State).

%% Give the Slack status back to the eTodo status.
timerDone(State = #state{timer = false}) ->
    State;
timerDone(State = #state{srvStatus = Status, srvStatusMsg = StatusMsg}) ->
    Profile = #{status_text       => statusText(StatusMsg),
                status_emoji      => statusEmoji(Status),
                status_expiration => 0},
    State2  = setProfile(Profile, State#state{timer = false}),
    case Status of
        "Busy" ->
            callApi("dnd.setSnooze", [{num_minutes, ?BusySnooze}], none,
                    State2);
        _ ->
            callApi("dnd.endSnooze", [], none, State2)
    end.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(_EScriptDir, _User, _Users, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives a system message.
%%
%% @spec eReceivedSysMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedSysMsg(_Dir, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%% Slackbot reminds the user, a message to oneself gives no notification.
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedAlarmMsg(_Dir, Text, State = #state{alarmsToSlack = true,
                                            myId          = MyId})
  when MyId =/= undefined ->
    callApi("reminders.add", [{text, Text}, {time, 30}], {alarm, Text}, State);
eReceivedAlarmMsg(_Dir, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs in.
%%
%% @spec eLoggedInMsg(Dir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eLoggedInMsg(_Dir, _User, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called when a peer connected to the users circle logs out.
%%
%% @spec eLoggedOutMsg(Dir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eLoggedOutMsg(_Dir, _User, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user sends a message.
%%
%% @spec eSendMsg(Dir, User, Users, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eSendMsg(_EScriptDir, _User, _Users, _Text, State = #state{userToken = ""}) ->
    State;
eSendMsg(_EScriptDir, User, Users, Text, State) ->
    postChatMessages(State, User, Users, Text).

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(_Dir, _User, Status, StatusMsg,
                 State = #state{userToken = ""}) ->
    State#state{srvStatus = Status, srvStatusMsg = StatusMsg};
eSetStatusUpdate(_Dir, _User, Status, StatusMsg,
                 State = #state{srvStatus = OldStatus, timer = Timer}) ->
    State2 = State#state{srvStatus = Status, srvStatusMsg = StatusMsg},
    State3 = case Timer of
                 true  -> State2;
                 false -> setProfile(profile(Status, StatusMsg, OldStatus),
                                     State2)
             end,
    State4 = callApi("users.setPresence", [{presence, mapStatus(Status)}],
                     none, State3),
    case {Timer, OldStatus, Status} of
        {true, _, _} ->
            State4;
        {false, "Busy", "Busy"} ->
            State4;
        {false, _, "Busy"} ->
            callApi("dnd.setSnooze", [{num_minutes, ?BusySnooze}], none,
                    State4);
        {false, "Busy", _} ->
            callApi("dnd.endSnooze", [], none, State4);
        _ ->
            State4
    end.

%% The status emoji is only changed when Busy starts or ends, so an
%% emoji chosen in Slack stays.
profile(Status, StatusMsg, OldStatus) ->
    Text = statusText(StatusMsg),
    case {Status, OldStatus, Text} of
        {"Busy", _, _}  -> #{status_text => Text, status_emoji => statusEmoji(Status)};
        {_, "Busy", _}  -> #{status_text => Text, status_emoji => <<>>};
        {_, _, <<>>}    -> #{status_text => <<>>, status_emoji => <<>>};
        _               -> #{status_text => Text}
    end.

statusEmoji("Busy")  -> <<":no_entry:">>;
statusEmoji(_Status) -> <<>>.

statusText(undefined) ->
    <<>>;
statusText(Text) ->
    string:slice(unicode:characters_to_binary(Text), 0, ?MaxStatus).

setProfile(Profile, State) ->
    Params = [{profile, jsx:encode(Profile)}],
    callApi("users.profile.set", Params, none, State#state{profile = Params}).

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes work log date
%% @spec eSetWorkLogDate(Dir, User, Date, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eSetWorkLogDate(_Dir, _User, Date, State) ->
    State#state{workLogDate = Date}.

%%--------------------------------------------------------------------
%% @doc
%% Called for right click menu
%%
%% @spec eMenuEvent(EScriptDir, User, MenuOption, ETodo, MenuText, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, ETodoUser, 1550, _ETodo, _MenuText, State) ->
    addContacts(ETodoUser, "Choose which users to add", "Add users",
                getUsers(State), State);
eMenuEvent(_EScriptDir, ETodoUser, 1551, _ETodo, _MenuText, State) ->
    addContacts(ETodoUser, "Choose which channels to add", "Add channel",
                getChannels(State), State);
eMenuEvent(_EScriptDir, _User, 1552, _ETodo, _MenuText,
           State = #state{frame = Frame}) ->
    case askToken(Frame, "Enter Slack user token (xoxp-...)",
                  State#state.userToken) of
        {ok, UserToken} ->
            case askToken(Frame, "Enter Slack app-level token (xapp-...)",
                          State#state.appToken) of
                {ok, AppToken} ->
                    Cfg = readConfig(),
                    ePluginInterface:saveConfig(?MODULE,
                                                Cfg#{userToken => UserToken,
                                                     appToken  => AppToken}),
                    self() ! slackStart,
                    loadConfig(State);
                cancel ->
                    State
            end;
        cancel ->
            State
    end;
eMenuEvent(_EScriptDir, _User, 1553, _ETodo, _MenuText, State) ->
    self() ! slackStart,
    State;
eMenuEvent(_EScriptDir, _User, 1554, _ETodo, _MenuText,
           State = #state{myId = undefined}) ->
    ePluginInterface:systemEntry("Slack: not connected"),
    State;
eMenuEvent(_EScriptDir, ETodoUser, 1554, _ETodo, _MenuText,
           State = #state{frame = Frame, myId = MyId}) ->
    Channels = memberChannels(State),
    Choices  = ["Myself (direct message)" |
                ["#" ++ unicode:characters_to_list(Name) || Name <- Channels]],
    ChoiceDlg = wxSingleChoiceDialog:new(Frame, "Post the work log to",
                                         "Post work log", Choices),
    State2 = case wxSingleChoiceDialog:showModal(ChoiceDlg) of
                 ?wxID_OK ->
                     Address = case wxSingleChoiceDialog:getSelection(ChoiceDlg) of
                                   0     -> "!" ++ binary_to_list(MyId);
                                   Index -> "#" ++ unicode:characters_to_list(
                                                     lists:nth(Index, Channels))
                               end,
                     Text = workLogText(ETodoUser, workLogDate(State)),
                     postTo(Address, Text, State);
                 _ ->
                     State
             end,
    wxSingleChoiceDialog:destroy(ChoiceDlg),
    State2;
eMenuEvent(_EScriptDir, _User, 1555, _ETodo, _MenuText,
           State = #state{timerStatus = On}) ->
    saveSetting(timerStatus, not On),
    State#state{timerStatus = not On};
eMenuEvent(_EScriptDir, _User, 1556, _ETodo, _MenuText,
           State = #state{alarmsToSlack = On}) ->
    saveSetting(alarmsToSlack, not On),
    State#state{alarmsToSlack = not On};
eMenuEvent(_EScriptDir, _User, _MenuEvent, _ETodo, _MenuText, State) ->
    State.

addContacts(ETodoUser, Message, Caption, Choices,
            State = #state{frame = Frame}) ->
    MultiDlg = wxMultiChoiceDialog:new(Frame, Message, Caption, Choices),
    wxMultiChoiceDialog:setSize(MultiDlg, {500, 500}),
    case wxMultiChoiceDialog:showModal(MultiDlg) of
        ?wxID_OK ->
            MSel = wxMultiChoiceDialog:getSelections(MultiDlg),
            [addExternalUser(ETodoUser, lists:nth(Index + 1, Choices))
             || Index <- MSel];
        ?wxID_CANCEL ->
            ok
    end,
    wxMultiChoiceDialog:destroy(MultiDlg),
    State.

askToken(Frame, Message, Value) ->
    EntryDlg = wxPasswordEntryDialog:new(Frame, Message,
                                         [{caption, "Slack settings"},
                                          {value,   Value}]),
    Result = case wxPasswordEntryDialog:showModal(EntryDlg) of
                 ?wxID_OK ->
                     {ok, string:trim(wxPasswordEntryDialog:getValue(EntryDlg))};
                 _ ->
                     cancel
             end,
    wxPasswordEntryDialog:destroy(EntryDlg),
    Result.

workLogDate(#state{workLogDate = undefined}) -> date();
workLogDate(#state{workLogDate = Date})      -> Date.

workLogText(User, Date = {Year, Month, Day}) ->
    Rows  = [{unicode:characters_to_binary(Desc), Hours * 60 + Minutes}
             || {Uid, Desc} <- ePluginInterface:getWorkLog(User, Date),
                {_, Hours, Minutes} <-
                    [ePluginInterface:getLoggedWork(User, Uid, Date)],
                Hours * 60 + Minutes > 0],
    Title = io_lib:format("*Work log ~4..0w-~2..0w-~2..0w*",
                          [Year, Month, Day]),
    Lines = [[<<"• "/utf8>>, escape(firstLine(Desc, ?MaxTaskDesc)),
              <<" — "/utf8>>, duration(Minutes)] || {Desc, Minutes} <- Rows],
    Total = lists:sum([Minutes || {_Desc, Minutes} <- Rows]),
    unicode:characters_to_binary(
      case Rows of
          [] -> [Title, "\nNo work logged."];
          _  -> [Title, "\n", lists:join("\n", Lines),
                 "\n*Total:* ", duration(Total)]
      end).

duration(Minutes) ->
    io_lib:format("~bh ~2..0bm", [Minutes div 60, Minutes rem 60]).

firstLine(Text, Max) ->
    [Line|_] = string:split(string:trim(Text), "\n"),
    string:slice(Line, 0, Max).

%%--------------------------------------------------------------------
%% @doc
%% Handle messages sent to this plugin
%%
%% @spec handleInfo(Info, State) -> NewState
%% @end
%%--------------------------------------------------------------------
handleInfo(slackStart, State) ->
    start(State);
handleInfo(slackReconnect, State = #state{wsConn = undefined}) ->
    openSocket(State);
handleInfo(slackPoll, State) ->
    erlang:send_after(?PresencePoll, self(), slackPoll),
    pollPresence(State);
handleInfo({slackRetry, {"users.profile.set", Params, _Token, _Ctx}},
           State = #state{profile = Profile}) when Params =/= Profile ->
    %% A newer status has been set since.
    State;
handleInfo({slackRetry, {Method, Params, Token, Ctx}}, State) ->
    callApi(Method, Params, Ctx, Token, State);
handleInfo({http, {ReqId, Result}}, State = #state{requests = Requests}) ->
    case maps:take(ReqId, Requests) of
        {Request, Requests2} ->
            handleHttp(Result, Request, State#state{requests = Requests2});
        error ->
            State
    end;
handleInfo({gun_up, Conn, _Protocol},
           State = #state{wsConn = Conn, wsPath = Path}) ->
    State#state{wsRef = gun:ws_upgrade(Conn, Path)};
handleInfo({gun_ws, Conn, Ref, {text, Body}},
           State = #state{wsConn = Conn, wsRef = Ref}) ->
    handleEnvelope(jsx:decode(Body, [return_maps]), State);
handleInfo({gun_ws, Conn, _Ref, close}, State = #state{wsConn = Conn}) ->
    reconnect(closed, State);
handleInfo({gun_ws, Conn, _Ref, {close, Code, _}},
           State = #state{wsConn = Conn}) ->
    reconnect({closed, Code}, State);
handleInfo({gun_response, Conn, _Ref, _IsFin, Status, _Headers},
           State = #state{wsConn = Conn}) ->
    reconnect({upgrade_failed, Status}, State);
handleInfo({gun_down, Conn, _Protocol, Reason, _Killed},
           State = #state{wsConn = Conn}) ->
    reconnect(Reason, State);
handleInfo({gun_error, Conn, _Ref, Reason}, State = #state{wsConn = Conn}) ->
    reconnect(Reason, State);
handleInfo({gun_error, Conn, Reason}, State = #state{wsConn = Conn}) ->
    reconnect(Reason, State);
handleInfo(_Info, State) ->
    State.

%%%===================================================================
%%% Connection
%%%===================================================================
start(State) ->
    State2 = closeSocket(State#state{myId     = undefined,
                                     presence = undefined,
                                     users    = #{},
                                     convs    = #{},
                                     ims      = #{},
                                     reported = #{},
                                     backoff  = ?MinBackoff}),
    case State2#state.userToken of
        "" -> State2;
        _  -> callApi("auth.test", [], none, State2)
    end.

openSocket(State = #state{appToken = ""}) ->
    report("apps.connections.open", {slack, <<"no app-level token, "
                                              "incoming messages are off">>},
           State);
openSocket(State = #state{appToken = AppToken}) ->
    callApi("apps.connections.open", [], none, AppToken, State).

wsConnect(Url, State) ->
    State2 = closeSocket(State),
    Uri    = uri_string:parse(binary_to_list(Url)),
    Path   = case maps:get(path, Uri, "") of
                 ""    -> "/";
                 Path0 -> Path0
             end,
    Query  = case maps:get(query, Uri, "") of
                 ""     -> "";
                 Query0 -> "?" ++ Query0
             end,
    {Transport, DefaultPort} = case maps:get(scheme, Uri, "wss") of
                                   "ws" -> {tcp, 80};
                                   _    -> {tls, 443}
                               end,
    Opts   = #{protocols => [http], transport => Transport, retry => 0},
    case gun:open(maps:get(host, Uri), maps:get(port, Uri, DefaultPort),
                  Opts) of
        {ok, Conn} ->
            State2#state{wsConn = Conn, wsPath = Path ++ Query};
        {error, Reason} ->
            reconnect(Reason, State2)
    end.

closeSocket(State = #state{wsConn = undefined}) ->
    State;
closeSocket(State = #state{wsConn = Conn}) ->
    gun:close(Conn),
    State#state{wsConn = undefined, wsRef = undefined}.

reconnect(Reason, State) ->
    ?log(reconnect, [Reason], "Slack connection lost, reconnecting"),
    retryLater(slackReconnect, closeSocket(State)).

retryLater(Msg, State = #state{backoff = Backoff}) ->
    erlang:send_after(Backoff, self(), Msg),
    State#state{backoff = min(2 * Backoff, ?MaxBackoff)}.

handleEnvelope(#{<<"type">> := <<"hello">>}, State) ->
    State#state{backoff = ?MinBackoff};
handleEnvelope(#{<<"type">>   := <<"disconnect">>,
                 <<"reason">> := <<"link_disabled">>}, State) ->
    report("apps.connections.open", {slack, <<"socket mode is disabled">>},
           closeSocket(State));
handleEnvelope(#{<<"type">> := <<"disconnect">>}, State) ->
    openSocket(closeSocket(State));
handleEnvelope(Envelope = #{<<"envelope_id">> := Id},
               State = #state{wsConn = Conn, wsRef = Ref}) ->
    gun:ws_send(Conn, Ref, {text, jsx:encode(#{envelope_id => Id})}),
    case Envelope of
        #{<<"type">>    := <<"events_api">>,
          <<"payload">> := #{<<"event">> := Event}} ->
            handleEvent(Event, State);
        _ ->
            State
    end;
handleEnvelope(_Envelope, State) ->
    State.

%% Edits are shown again, unless only a link preview was added.
handleEvent(Event = #{<<"type">>             := <<"message">>,
                      <<"subtype">>          := <<"message_changed">>,
                      <<"channel">>          := Channel,
                      <<"message">>          := Message = #{<<"user">> := _,
                                                            <<"text">> := Text},
                      <<"previous_message">> := #{<<"text">> := OldText}},
            State) when Text =/= OldText ->
    Edited = maps:merge(maps:with([<<"channel_type">>], Event),
                        maps:remove(<<"files">>, Message)),
    handleEvent(Edited#{<<"type">>    => <<"message">>,
                        <<"channel">> => Channel,
                        <<"edited">>  => true}, State);
handleEvent(Event = #{<<"type">>    := <<"message">>,
                      <<"user">>    := User,
                      <<"channel">> := Channel}, State) ->
    case {showMessage(Event, User, State),
          maps:is_key(User, State#state.users)} of
        {false, _} ->
            State;
        {true, true} ->
            deliver(Event, User, Channel, State);
        {true, false} ->
            callApi("users.info", [{user, User}], {deliver, Event}, State)
    end;
handleEvent(#{<<"type">> := <<"user_change">>,
              <<"user">> := UserMap = #{<<"id">> := Id}}, State) ->
    State2 = State#state{users = maps:put(Id, UserMap, State#state.users)},
    case {Id == State#state.myId, State#state.timer} of
        {true, false} ->
            Profile = maps:get(<<"profile">>, UserMap, #{}),
            State2#state{statusText = maps:get(<<"status_text">>, Profile, <<>>)};
        _ ->
            State2
    end;
%% Do Not Disturb started in Slack makes eTodo Busy and the other way
%% around. Not while a timer runs, then the snooze is ours.
handleEvent(#{<<"type">>       := <<"dnd_updated">>,
              <<"dnd_status">> := Dnd},
            State = #state{timer = false, srvStatus = SrvStatus}) ->
    case {inDnd(Dnd), SrvStatus} of
        {true,  "Available"} -> State#state{status = "Busy"};
        {false, "Busy"}      -> State#state{status = "Available"};
        _                    -> State
    end;
handleEvent(#{<<"type">>     := <<"reaction_added">>,
              <<"user">>     := MyId,
              <<"reaction">> := Reaction,
              <<"item">>     := #{<<"type">>    := <<"message">>,
                                  <<"channel">> := Channel,
                                  <<"ts">>      := Ts}},
            State = #state{myId = MyId, taskReaction = Reaction}) ->
    callApi("conversations.replies", [{channel, Channel}, {ts, Ts},
                                      {limit, 200}],
            {task, Channel, Ts}, State);
handleEvent(_Event, State) ->
    State.

inDnd(#{<<"snooze_enabled">> := true}) ->
    true;
inDnd(Dnd = #{<<"dnd_enabled">> := true}) ->
    Now = erlang:system_time(second),
    (maps:get(<<"next_dnd_start_ts">>, Dnd, Now + 1) =< Now) andalso
        (Now < maps:get(<<"next_dnd_end_ts">>, Dnd, 0));
inDnd(_Dnd) ->
    false.

pollPresence(State = #state{myId = undefined}) ->
    State;
pollPresence(State = #state{myId = MyId}) ->
    callApi("users.getPresence", [{user, MyId}], none, State).

presence(Presence, State = #state{presence = Presence}) ->
    State;
presence(Presence, State = #state{presence = undefined}) ->
    State#state{presence = Presence};
presence(Presence, State) ->
    State#state{presence = Presence,
                status   = mapStatus(Presence, State#state.srvStatus)}.

%%%===================================================================
%%% Web API
%%%===================================================================
callApi(Method, Params, Ctx, State) ->
    callApi(Method, Params, Ctx, State#state.userToken, State).

callApi(Method, Params, Ctx, Token,
        State = #state{slackUrl = Url, requests = Requests}) ->
    Request = {Method, Params, Token, Ctx},
    Body    = uri_string:compose_query([{atom_to_binary(Key), toBin(Value)}
                                        || {Key, Value} <- Params]),
    HttpReq = {Url ++ "/" ++ Method,
               [{"authorization", "Bearer " ++ Token}],
               "application/x-www-form-urlencoded; charset=utf-8", Body},
    case httpc:request(post, HttpReq, httpOptions(),
                       [{sync, false}, {body_format, binary}]) of
        {ok, ReqId} ->
            State#state{requests = Requests#{ReqId => Request}};
        {error, Reason} ->
            failed(Request, Reason, State)
    end.

httpOptions() ->
    [{timeout, ?Timeout}, {ssl, httpc:ssl_verify_host_options(true)}].

toBin(Value) when is_binary(Value)  -> Value;
toBin(Value) when is_integer(Value) -> integer_to_binary(Value);
toBin(Value) when is_atom(Value)    -> atom_to_binary(Value);
toBin(Value)                        -> unicode:characters_to_binary(Value).

handleHttp({{_, 200, _}, _Headers, Picture}, {portrait, Name}, State) ->
    ePluginInterface:setPortrait(unicode:characters_to_list(Name),
                                 Picture, false),
    State;
handleHttp(_Result, {portrait, _Name}, State) ->
    State;
handleHttp({{_, 200, _}, Headers, Data}, {file, From, To, File}, State) ->
    %% Slack answers with its login page when the token can't read files.
    LoginPage = lists:prefix("text/html",
                             proplists:get_value("content-type", Headers, ""))
        andalso maps:get(<<"mimetype">>, File, <<>>) =/= <<"text/html">>,
    case LoginPage orelse storeFile(File, Data) of
        {ok, FileMD} -> ePluginInterface:msgEntry(From, To, FileMD);
        _            -> fileLink(File, From, To)
    end,
    State;
handleHttp(_Result, {file, From, To, File}, State) ->
    fileLink(File, From, To),
    State;
handleHttp({{_, 200, _}, _Headers, Body}, Request = {Method, _, _, Ctx},
           State) ->
    case jsx:decode(Body, [return_maps]) of
        JSON = #{<<"ok">> := true} ->
            handleResponse(Method, JSON, Ctx, State);
        JSON ->
            failed(Request, {slack, slackError(JSON)}, State)
    end;
handleHttp({{_, 429, _}, Headers, _Body}, Request, State) ->
    erlang:send_after(retryAfter(Headers) * 1000, self(),
                      {slackRetry, Request}),
    State;
handleHttp({{_, Status, _}, _Headers, _Body}, Request, State) ->
    failed(Request, {http, Status}, State);
handleHttp({error, Reason}, Request, State) ->
    failed(Request, Reason, State).

retryAfter(Headers) ->
    case string:to_integer(proplists:get_value("retry-after", Headers, "")) of
        {Seconds, _} when is_integer(Seconds) -> Seconds;
        _                                     -> 30
    end.

slackError(JSON = #{<<"error">> := <<"missing_scope">>}) ->
    Needed = maps:get(<<"needed">>, JSON, <<"?">>),
    <<"missing_scope ", Needed/binary>>;
slackError(JSON) ->
    maps:get(<<"error">>, JSON, <<"unknown_error">>).

handleResponse("auth.test", #{<<"user_id">> := MyId}, _Ctx, State) ->
    State2 = State#state{myId = MyId, backoff = ?MinBackoff},
    openSocket(listConversations(<<>>, listUsers(<<>>, State2)));
handleResponse("users.list", JSON, _Ctx, State) ->
    Users = lists:foldl(fun(User = #{<<"id">> := Id}, Acc) ->
                                Acc#{Id => User}
                        end, State#state.users,
                        maps:get(<<"members">>, JSON, [])),
    nextPage(fun listUsers/2, JSON, State#state{users = Users});
handleResponse("conversations.list", JSON, _Ctx, State) ->
    State2 = lists:foldl(fun addConversation/2, State,
                         maps:get(<<"channels">>, JSON, [])),
    nextPage(fun listConversations/2, JSON, State2);
handleResponse("users.info", #{<<"user">> := User = #{<<"id">> := Id}},
               Ctx, State) ->
    State2 = State#state{users = maps:put(Id, User, State#state.users)},
    case Ctx of
        {deliver, Event} -> handleEvent(Event, State2);
        _                -> State2
    end;
handleResponse("conversations.open", #{<<"channel">> := #{<<"id">> := Channel}},
               {post, User, Text}, State) ->
    Ims = maps:put(User, Channel, State#state.ims),
    postMessage(Channel, Text, State#state{ims = Ims});
%% Replying from eTodo means the conversation has been read.
handleResponse("chat.postMessage", #{<<"channel">> := Channel,
                                     <<"ts">>      := Ts}, _Ctx, State) ->
    callApi("conversations.mark", [{channel, Channel}, {ts, Ts}], none, State);
handleResponse("conversations.replies", JSON, {task, Channel, Ts}, State) ->
    case [Message || Message = #{<<"ts">> := MsgTs}
                         <- maps:get(<<"messages">>, JSON, []), MsgTs == Ts] of
        [Message|_] ->
            callApi("chat.getPermalink", [{channel, Channel}, {message_ts, Ts}],
                    {task, Channel, Message}, State);
        [] ->
            State
    end;
handleResponse("chat.getPermalink", #{<<"permalink">> := Link},
               {task, Channel, Message}, State) ->
    createTask(Message, Channel, Link, State);
handleResponse("apps.connections.open", #{<<"url">> := Url}, _Ctx, State) ->
    wsConnect(Url, State);
handleResponse("users.getPresence", #{<<"presence">> := Presence}, _Ctx,
               State) ->
    presence(Presence, State);
handleResponse(_Method, _JSON, _Ctx, State) ->
    State.

failed(Request = {Method, _Params, _Token, Ctx}, Reason, State) ->
    ?log(failed, [Method, Reason], "Slack request failed"),
    giveUpOrRetry(Request, Ctx, Reason, report(Method, Reason, State)).

%% Network trouble at start up is retried, rejected tokens are not.
giveUpOrRetry({"auth.test", _, _, _}, _Ctx, {slack, _}, State) ->
    State;
giveUpOrRetry({"auth.test", _, _, _}, _Ctx, _Reason, State) ->
    retryLater(slackStart, State);
giveUpOrRetry({"apps.connections.open", _, _, _}, _Ctx, {slack, _}, State) ->
    State;
giveUpOrRetry({"apps.connections.open", _, _, _}, _Ctx, _Reason, State) ->
    retryLater(slackReconnect, State);
giveUpOrRetry(_Request, {deliver, Event = #{<<"user">> := User}}, _Reason,
              State) ->
    Users = maps:put(User, #{<<"id">> => User}, State#state.users),
    handleEvent(Event, State#state{users = Users});
giveUpOrRetry(_Request, {task, Channel, Message}, _Reason, State)
  when is_map(Message) ->
    createTask(Message, Channel, undefined, State);
giveUpOrRetry(_Request, {alarm, Text}, _Reason, State = #state{myId = MyId}) ->
    postTo("!" ++ binary_to_list(MyId), alarmText(Text), State);
giveUpOrRetry(_Request, _Ctx, _Reason, State) ->
    State.

alarmText(Text) ->
    <<"⏰ "/utf8, (escape(unicode:characters_to_binary(Text)))/binary>>.

%% Errors reported by Slack are shown once in eTodo.
report(_Method, {slack, <<"snooze_not_active">>}, State) ->
    State;
report(Method, {slack, Error}, State = #state{reported = Reported}) ->
    case maps:is_key({Method, Error}, Reported) of
        true ->
            State;
        false ->
            BinMethod = list_to_binary(Method),
            ePluginInterface:systemEntry(<<"Slack: ", BinMethod/binary,
                                           " failed, ", Error/binary>>),
            State#state{reported = Reported#{{Method, Error} => true}}
    end;
report(_Method, _Reason, State) ->
    State.

nextPage(List, #{<<"response_metadata">> := #{<<"next_cursor">> := Cursor}},
         State) when Cursor =/= <<>> ->
    List(Cursor, State);
nextPage(_List, _JSON, State) ->
    State.

listUsers(Cursor, State) ->
    callApi("users.list", [{limit, 200} | cursor(Cursor)], none, State).

listConversations(Cursor, State) ->
    Types = <<"public_channel,private_channel,mpim,im">>,
    callApi("conversations.list",
            [{types, Types}, {exclude_archived, true}, {limit, 200} |
             cursor(Cursor)], none, State).

cursor(<<>>)   -> [];
cursor(Cursor) -> [{cursor, Cursor}].

addConversation(Conv = #{<<"id">>    := Id,
                         <<"is_im">> := true,
                         <<"user">>  := User}, State) ->
    State#state{convs = maps:put(Id, Conv, State#state.convs),
                ims   = maps:put(User, Id, State#state.ims)};
addConversation(Conv = #{<<"id">> := Id}, State) ->
    State#state{convs = maps:put(Id, Conv, State#state.convs)}.

%%%===================================================================
%%% Messages to Slack
%%%===================================================================
postChatMessages(State, User, Users, Text) ->
    Contacts  = externalUsers(User),
    SlackText = mdToSlack(unicode:characters_to_binary(Text)),
    lists:foldl(fun(Recipient, StateAcc) ->
                        postTo(slackAddress(Recipient, Contacts), SlackText,
                               StateAcc)
                end, State, string:tokens(Users, ";")).

slackAddress(Recipient, Contacts) ->
    case [Address || Contact <- Contacts,
                     {Desc, Address} <- [ePluginInterface:getPeerInfo(Contact)],
                     Desc == Recipient, isSlackAddress(Address)] of
        [Address|_] -> Address;
        []          -> none
    end.

isSlackAddress("!" ++ _) -> true;
isSlackAddress("#" ++ _) -> true;
isSlackAddress(_)        -> false.

%% Text is in Slack's mrkdwn format.
postTo(none, _Text, State) ->
    State;
postTo("#" ++ Name, Text, State) ->
    postMessage(channelId(unicode:characters_to_binary(Name), State),
                Text, State);
postTo("!" ++ Id, Text, State = #state{ims = Ims}) ->
    BinId = unicode:characters_to_binary(Id),
    case {isUserId(BinId), maps:find(BinId, Ims)} of
        {true, {ok, Channel}} ->
            postMessage(Channel, Text, State);
        {true, error} ->
            callApi("conversations.open", [{users, BinId}],
                    {post, BinId, Text}, State);
        {false, _} ->
            postMessage(BinId, Text, State)
    end.

isUserId(<<"U", _/binary>>) -> true;
isUserId(<<"W", _/binary>>) -> true;
isUserId(_)                 -> false.

postMessage(Channel, Text, State) ->
    callApi("chat.postMessage", [{channel, Channel}, {text, Text}],
            none, State).

channelId(Name, #state{convs = Convs}) ->
    case [Id || {Id, #{<<"name">> := ConvName}} <- maps:to_list(Convs),
                ConvName == Name] of
        [Id|_] -> Id;
        []     -> <<"#", Name/binary>>
    end.

%%%===================================================================
%%% Messages from Slack
%%%===================================================================

%% Messages written in eTodo come back from Slack, they lack the
%% client_msg_id that Slack's own clients set.
showMessage(Event, User, #state{myId = MyId}) ->
    Subtype = maps:get(<<"subtype">>, Event, none),
    Shown   = [none, <<"file_share">>, <<"thread_broadcast">>,
               <<"me_message">>],
    Echo    = (User == MyId) andalso
              not maps:is_key(<<"client_msg_id">>, Event) andalso
              not maps:is_key(<<"attachments">>, Event) andalso
              not maps:is_key(<<"files">>, Event),
    lists:member(Subtype, Shown) andalso not Echo.

deliver(Event, User, Channel, State) ->
    State2 = rememberIm(Event, User, Channel, State),
    From   = userName(User, State2),
    To     = [contact(Event, User, Channel, State2)],
    sendMsgEntry(Event, From, To, State2),
    State3 = lists:foldl(fun(File, StateAcc) ->
                                 fetchFile(File, From, To, StateAcc)
                         end, State2, maps:get(<<"files">>, Event, [])),
    fetchPortrait(User, From, State3).

rememberIm(Event, User, Channel, State = #state{myId = MyId, ims = Ims}) ->
    case channelType(Event, Channel, State) of
        <<"im">> when User =/= MyId ->
            State#state{ims = maps:put(User, Channel, Ims)};
        _ ->
            State
    end.

channelType(Event, Channel, #state{convs = Convs}) ->
    case maps:get(<<"channel_type">>, Event, undefined) of
        undefined ->
            case maps:get(Channel, Convs, #{}) of
                #{<<"is_im">>   := true} -> <<"im">>;
                #{<<"is_mpim">> := true} -> <<"mpim">>;
                _                        -> <<"channel">>
            end;
        Type ->
            Type
    end.

%% The eTodo contact a conversation is shown under. Direct messages
%% use "!UserId", contacts added before used "!ChannelId".
contact(Event, User, Channel, State) ->
    ChannelAddr = <<"!", Channel/binary>>,
    case channelType(Event, Channel, State) of
        <<"im">> ->
            case imPeer(User, Channel, State) of
                undefined ->
                    findContact([ChannelAddr], <<"Slack - ", Channel/binary>>,
                                ChannelAddr);
                Peer ->
                    PeerName = userName(Peer, State),
                    PeerAddr = <<"!", Peer/binary>>,
                    findContact([PeerAddr, ChannelAddr],
                                <<"Slack - ", PeerName/binary>>, PeerAddr)
            end;
        <<"mpim">> ->
            Name = mpimName(Channel, State),
            findContact([ChannelAddr], <<"Slack - ", Name/binary>>,
                        ChannelAddr);
        _ ->
            Name     = channelName(Channel, State),
            NameAddr = <<"#", Name/binary>>,
            findContact([NameAddr, ChannelAddr], <<"Slack - ", Name/binary>>,
                        NameAddr)
    end.

imPeer(User, _Channel, #state{myId = MyId}) when User =/= MyId ->
    User;
imPeer(_User, Channel, #state{ims = Ims}) ->
    case [Peer || {Peer, Id} <- maps:to_list(Ims), Id == Channel] of
        [Peer|_] -> Peer;
        []       -> undefined
    end.

findContact(Addresses, Desc, Address) ->
    case ePluginInterface:loggedIn() of
        false ->
            Desc;
        {true, ETodoUser} ->
            Contacts = [{unicode:characters_to_binary(CDesc),
                         unicode:characters_to_binary(CAddr)}
                        || Contact <- externalUsers(ETodoUser),
                           {CDesc, CAddr} <-
                               [ePluginInterface:getPeerInfo(Contact)]],
            case [CDesc || Addr <- Addresses, {CDesc, CAddr} <- Contacts,
                           CAddr == Addr] of
                [Found|_] ->
                    Found;
                [] ->
                    ePluginInterface:saveExternalUser(ETodoUser, Desc, Address),
                    Desc
            end
    end.

externalUsers(ETodoUser) ->
    case ePluginInterface:getExternalUsers(ETodoUser) of
        Contacts when is_list(Contacts) -> Contacts;
        _                               -> []
    end.

channelName(Channel, #state{convs = Convs}) ->
    case maps:get(Channel, Convs, #{}) of
        #{<<"name">> := Name} -> Name;
        _                     -> Channel
    end.

%% "mpdm-anna--bo--carl-1" -> "anna, bo, carl"
mpimName(Channel, #state{convs = Convs}) ->
    case maps:get(Channel, Convs, #{}) of
        #{<<"name">> := <<"mpdm-", Name/binary>>} ->
            Name2 = re:replace(Name, "-[0-9]+$", "", [{return, binary}]),
            iolist_to_binary(lists:join(<<", ">>,
                                        binary:split(Name2, <<"--">>,
                                                     [global])));
        _ ->
            Channel
    end.

conversationName(Channel, State) ->
    case channelType(#{}, Channel, State) of
        <<"im">>   -> <<"a direct message">>;
        <<"mpim">> -> mpimName(Channel, State);
        _          -> <<"#", (channelName(Channel, State))/binary>>
    end.

userName(Id, #state{users = Users}) ->
    User    = maps:get(Id, Users, #{}),
    Profile = maps:get(<<"profile">>, User, #{}),
    firstName([maps:get(<<"display_name">>, Profile, <<>>),
               maps:get(<<"real_name">>,    Profile, <<>>),
               maps:get(<<"name">>,         User,    <<>>)], Id).

firstName([],             Id) -> Id;
firstName([<<>> | Rest],  Id) -> firstName(Rest, Id);
firstName([Name | _Rest], _Id) -> Name.

%% Name lookup for slackToMD/2.
names(State = #state{users = Users}) ->
    fun({user, Id}) ->
            case maps:is_key(Id, Users) of
                true  -> userName(Id, State);
                false -> undefined
            end;
       ({channel, Id}) ->
            case channelName(Id, State) of
                Id   -> undefined;
                Name -> Name
            end
    end.

fetchPortrait(User, Name, State = #state{portraits = Portraits,
                                         requests  = Requests}) ->
    Profile = maps:get(<<"profile">>, maps:get(User, State#state.users, #{}),
                       #{}),
    case {maps:is_key(User, Portraits), maps:get(<<"image_72">>, Profile, none)} of
        {false, Url} when is_binary(Url) ->
            case httpc:request(get, {binary_to_list(Url), []}, httpOptions(),
                               [{sync, false}, {body_format, binary}]) of
                {ok, ReqId} ->
                    State#state{portraits = Portraits#{User => true},
                                requests  = Requests#{ReqId => {portrait, Name}}};
                {error, _Reason} ->
                    State
            end;
        _ ->
            State
    end.

%% Files are stored as linked files in eTodo and shown in a message of
%% their own when downloaded.
fetchFile(File, From, To, State = #state{userToken = Token,
                                         requests  = Requests}) ->
    Url = maps:get(<<"url_private_download">>, File,
                   maps:get(<<"url_private">>, File, undefined)),
    case is_binary(Url) andalso
        (maps:get(<<"size">>, File, 0) =< ?MaxFileSize) of
        true ->
            HttpReq = {binary_to_list(Url),
                       [{"authorization", "Bearer " ++ Token}]},
            case httpc:request(get, HttpReq, httpOptions(),
                               [{sync, false}, {body_format, binary}]) of
                {ok, ReqId} ->
                    Request = {file, From, To, File},
                    State#state{requests = Requests#{ReqId => Request}};
                {error, _Reason} ->
                    fileLink(File, From, To),
                    State
            end;
        false ->
            fileLink(File, From, To),
            State
    end.

storeFile(File, Data) ->
    Name = fileName(File),
    case ePluginInterface:loggedIn() of
        {true, ETodoUser} ->
            case ePluginInterface:linkFile(ETodoUser, Name, Data) of
                {ok, _, Url, true} ->
                    {ok, iolist_to_binary(["![", Name, "](", Url, ")"])};
                {ok, _, Url, false} ->
                    {ok, iolist_to_binary(["[", Name, "](", Url, ")"])};
                error ->
                    error
            end;
        false ->
            error
    end.

fileName(File) ->
    Name  = unicode:characters_to_list(maps:get(<<"name">>, File, <<>>)),
    Name2 = [case Char of
                 $/  -> $_;
                 $\\ -> $_;
                 _   -> Char
             end || Char <- Name, Char >= 32],
    case string:trim(Name2, leading, ".") of
        ""    -> "file";
        Name3 -> Name3
    end.

fileLink(File = #{<<"permalink">> := Link}, From, To) ->
    Name = unicode:characters_to_binary(fileName(File)),
    ePluginInterface:msgEntry(From, To, <<"[", Name/binary, "](",
                                          Link/binary, ")">>);
fileLink(_File, _From, _To) ->
    ok.

sendMsgEntry(Event, From, To, State) ->
    Text  = slackToMD(maps:get(<<"text">>, Event, <<>>), names(State)),
    Parts = [threadNote(Event), editNote(Event),
             attachmentMD(Event, State), Text],
    case [Part || Part <- Parts, Part =/= <<>>] of
        []       -> ok;
        MsgParts -> ePluginInterface:msgEntry(
                      From, To, iolist_to_binary(lists:join(<<"\n\n">>,
                                                            MsgParts)))
    end.

threadNote(#{<<"thread_ts">> := ThreadTs, <<"ts">> := Ts})
  when ThreadTs =/= Ts ->
    <<"↳ _Reply in a thread_"/utf8>>;
threadNote(_Event) ->
    <<>>.

editNote(#{<<"edited">> := true}) -> <<"✏️ _Edited_"/utf8>>;
editNote(_Event)                  -> <<>>.

attachmentMD(#{<<"attachments">> := [#{<<"image_url">> := Image,
                                       <<"title">>     := Title,
                                       <<"footer">>    := Footer}|_]},
             State) ->
    Footer2 = slackToMD(Footer, names(State)),
    <<"### [", Title/binary, "](", Image/binary, ")\n",
      "![", Title/binary, "](", Image/binary, ")\n\n",
      "*", Footer2/binary, "*">>;
attachmentMD(#{<<"attachments">> := [#{<<"image_url">> := Image,
                                       <<"footer">>    := Footer}|_]},
             State) ->
    Footer2 = slackToMD(Footer, names(State)),
    <<"![", Footer/binary, "](", Image/binary, ")\n\n",
      "*", Footer2/binary, "*">>;
attachmentMD(#{<<"attachments">> := [#{<<"image_url">> := Image}|_]},
             _State) ->
    <<"![Image](", Image/binary, ")">>;
attachmentMD(_Event, _State) ->
    <<>>.

%%%===================================================================
%%% Tasks from Slack
%%%===================================================================
createTask(Message, Channel, Link, State) ->
    case ePluginInterface:loggedIn() of
        false ->
            State;
        {true, User} ->
            Text    = slackToMD(maps:get(<<"text">>, Message, <<>>),
                                names(State)),
            Sender  = userName(maps:get(<<"user">>, Message, <<>>), State),
            Source  = <<"Slack message from ", Sender/binary, " in ",
                        (conversationName(Channel, State))/binary>>,
            Desc    = case firstLine(Text, ?MaxTaskDesc) of
                          <<>>  -> Source;
                          Line  -> Line
                      end,
            Comment = [Source, linkText(Link), "\n\n", Text],
            Todo    = #todo{uid         = ePluginInterface:makeRef(),
                            status      = planning,
                            priority    = medium,
                            description = unicode:characters_to_list(Desc),
                            comment     = unicode:characters_to_list(Comment),
                            progress    = 0,
                            createTime  = ePluginInterface:dateTime()},
            TaskList = ePluginInterface:getTaskList(),
            Row      = ePluginInterface:getRow(User, TaskList),
            Parent   = ePluginInterface:tryInt(TaskList),
            ePluginInterface:addTodo(#userInfo{userName = User,
                                               uid      = Todo#todo.uid,
                                               row      = Row,
                                               parent   = Parent}, Todo),
            ePluginInterface:todoCreated(TaskList, Row, Todo),
            ePluginInterface:systemEntry(<<"Slack: created the task \"",
                                           Desc/binary, "\"">>),
            State
    end.

linkText(undefined) -> <<>>;
linkText(Link)      -> <<"\n<", Link/binary, ">">>.

memberChannels(#state{convs = Convs}) ->
    lists:sort([Name || Conv = #{<<"name">> := Name} <- maps:values(Convs),
                        maps:get(<<"is_member">>, Conv, false),
                        not maps:get(<<"is_mpim">>, Conv, false)]).

%%%===================================================================
%%% Slack mrkdwn <-> Markdown
%%%===================================================================

%% Names(Ref) looks up {user, Id} and {channel, Id}, undefined when
%% unknown.
slackToMD(Text, Names) ->
    try
        iolist_to_binary([segmentToMD(Segment, Names)
                          || Segment <- codeSegments(Text)])
    catch
        _:_ ->
            Text
    end.

segmentToMD({block, Code}, _Names) ->
    [<<"\n```\n">>, string:trim(unescape(Code), both, "\n"), <<"\n```\n">>];
segmentToMD({code, Code}, _Names) ->
    [<<"`">>, unescape(Code), <<"`">>];
segmentToMD({text, Text}, Names) ->
    [case Part of
         <<"<", Ref/binary>> when byte_size(Ref) > 0 ->
             refToMD(binary:part(Ref, 0, byte_size(Ref) - 1), Names);
         _ ->
             emoji(styleToMD(unescape(Part)))
     end || Part <- re:split(Text, "(<[^<>\n]+>)", [{return, binary}]),
            Part =/= <<>>].

%% <@U123>, <#C123|name>, <!here>, <https://x|text>
refToMD(<<"@", Ref/binary>>, Names) ->
    [<<"@">>, label(Ref, fun(Id) -> Names({user, Id}) end)];
refToMD(<<"#", Ref/binary>>, Names) ->
    [<<"#">>, label(Ref, fun(Id) -> Names({channel, Id}) end)];
refToMD(<<"!", Ref/binary>>, _Names) ->
    case binary:split(Ref, <<"|">>) of
        [<<"subteam^", _/binary>>, Label] -> unescape(Label);
        [<<"date^", _/binary>>, Label]    -> unescape(Label);
        [Special | _] -> [<<"@">>, hd(binary:split(Special, <<"^">>))]
    end;
refToMD(Ref, _Names) ->
    case binary:split(Ref, <<"|">>) of
        [Url, Label] -> [<<"[">>, unescape(Label), <<"](">>, unescape(Url), <<")">>];
        [Url]        -> [<<"[">>, unescape(Url), <<"](">>, unescape(Url), <<")">>]
    end.

label(Ref, Lookup) ->
    case binary:split(Ref, <<"|">>) of
        [_Id, Label] when Label =/= <<>> ->
            unescape(Label);
        [Id | _] ->
            case Lookup(Id) of
                undefined -> Id;
                Name      -> Name
            end
    end.

styleToMD(Text) ->
    Bold = replace(Text, "\\*(?=\\S)([^*\\n]*?\\S)\\*", "**\\1**"),
    replace(Bold, "_(?=\\S)([^_\\n]*?\\S)_", "*\\1*").

mdToSlack(Text) ->
    try
        iolist_to_binary([segmentToSlack(Segment)
                          || Segment <- codeSegments(Text)])
    catch
        _:_ ->
            escape(Text)
    end.

segmentToSlack({block, Code}) ->
    [<<"```">>, escape(Code), <<"```">>];
segmentToSlack({code, Code}) ->
    [<<"`">>, escape(Code), <<"`">>];
segmentToSlack({text, Text}) ->
    Links = "!?\\[([^\\]\\n]*)\\]\\(([^)\\s]+)\\)",
    case re:run(Text, Links, [{capture, all, binary}, unicode]) of
        {match, [All, Label, Url]} ->
            [Before, After] = binary:split(Text, All),
            [styleToSlack(escape(Before)),
             <<"<">>, Url, <<"|">>, escape(Label), <<">">>,
             segmentToSlack({text, After})];
        nomatch ->
            styleToSlack(escape(Text))
    end.

%% **bold** -> *bold*, *italic* -> _italic_, ~~strike~~ -> ~strike~,
%% # Heading -> *Heading*
styleToSlack(Text) ->
    Bold1   = replace(Text,  "\\*\\*(?=\\S)(.+?)\\*\\*", "\x{1}\\1\x{1}"),
    Bold2   = replace(Bold1, "__(?=\\S)(.+?)__", "\x{1}\\1\x{1}"),
    Italic  = replace(Bold2, "\\*(?=\\S)([^*\\n]*?\\S)\\*", "_\\1_"),
    Strike  = replace(Italic, "~~(?=\\S)(.+?)~~", "~\\1~"),
    Heading = replace(Strike, "^#{1,6}[ \\t]+(.+?)[ \\t#]*$", "\x{1}\\1\x{1}"),
    binary:replace(Heading, <<1>>, <<"*">>, [global]).

replace(Text, Regexp, Replacement) ->
    Boundary = "(?<![\\w*_~])",
    re:replace(Text, Boundary ++ Regexp, Replacement,
               [global, multiline, unicode, ucp, {return, binary}]).

%% Splits Text into [{text | code | block, Binary}] on ``` and `.
codeSegments(Text) ->
    blocks(binary:split(Text, <<"```">>, [global]), text).

blocks([], _Mode) ->
    [];
blocks([Unclosed], block) ->
    inline(<<"```", Unclosed/binary>>);
blocks([Text | Rest], text) ->
    inline(Text) ++ blocks(Rest, block);
blocks([Code | Rest], block) ->
    [{block, Code} | blocks(Rest, text)].

inline(Text) ->
    spans(binary:split(Text, <<"`">>, [global]), text).

spans([], _Mode) ->
    [];
spans([Unclosed], code) ->
    [{text, <<"`", Unclosed/binary>>}];
spans([Text | Rest], text) ->
    [{text, Text} | spans(Rest, code)];
spans([Code | Rest], code) ->
    [{code, Code} | spans(Rest, text)].

escape(Text) ->
    lists:foldl(fun({From, To}, Acc) ->
                        binary:replace(Acc, From, To, [global])
                end, Text, [{<<"&">>, <<"&amp;">>},
                            {<<"<">>, <<"&lt;">>},
                            {<<">">>, <<"&gt;">>}]).

unescape(Text) ->
    lists:foldl(fun({From, To}, Acc) ->
                        binary:replace(Acc, From, To, [global])
                end, Text, [{<<"&lt;">>, <<"<">>},
                            {<<"&gt;">>, <<">">>},
                            {<<"&amp;">>, <<"&">>}]).

%% :tada: -> 🎉, unknown codes are kept.
emoji(Text) ->
    case re:run(Text, ":[a-z0-9_+-]+:", [global, {capture, first, index}]) of
        nomatch ->
            Text;
        {match, Matches} ->
            emoji(Text, [Match || [Match] <- Matches], 0, [])
    end.

emoji(Text, [], Pos, Acc) ->
    iolist_to_binary(lists:reverse([binary:part(Text, Pos, byte_size(Text) - Pos)
                                    | Acc]));
emoji(Text, [{Start, Len} | Rest], Pos, Acc) ->
    Code = binary:part(Text, Start + 1, Len - 2),
    case maps:find(Code, emojiTable()) of
        {ok, Emoji} ->
            emoji(Text, Rest, Start + Len,
                  [Emoji, binary:part(Text, Pos, Start - Pos) | Acc]);
        error ->
            emoji(Text, Rest, Pos, Acc)
    end.

emojiTable() ->
    #{<<"grinning">> => <<"😀"/utf8>>, <<"smiley">> => <<"😃"/utf8>>,
      <<"smile">> => <<"😄"/utf8>>, <<"grin">> => <<"😁"/utf8>>,
      <<"laughing">> => <<"😆"/utf8>>, <<"satisfied">> => <<"😆"/utf8>>,
      <<"sweat_smile">> => <<"😅"/utf8>>, <<"joy">> => <<"😂"/utf8>>,
      <<"rolling_on_the_floor_laughing">> => <<"🤣"/utf8>>,
      <<"slightly_smiling_face">> => <<"🙂"/utf8>>,
      <<"simple_smile">> => <<"🙂"/utf8>>,
      <<"upside_down_face">> => <<"🙃"/utf8>>, <<"wink">> => <<"😉"/utf8>>,
      <<"blush">> => <<"😊"/utf8>>, <<"innocent">> => <<"😇"/utf8>>,
      <<"smiling_face_with_3_hearts">> => <<"🥰"/utf8>>,
      <<"heart_eyes">> => <<"😍"/utf8>>, <<"star-struck">> => <<"🤩"/utf8>>,
      <<"kissing_heart">> => <<"😘"/utf8>>, <<"yum">> => <<"😋"/utf8>>,
      <<"stuck_out_tongue">> => <<"😛"/utf8>>,
      <<"stuck_out_tongue_winking_eye">> => <<"😜"/utf8>>,
      <<"stuck_out_tongue_closed_eyes">> => <<"😝"/utf8>>,
      <<"hugging_face">> => <<"🤗"/utf8>>, <<"hugs">> => <<"🤗"/utf8>>,
      <<"shushing_face">> => <<"🤫"/utf8>>,
      <<"thinking_face">> => <<"🤔"/utf8>>,
      <<"zipper_mouth_face">> => <<"🤐"/utf8>>,
      <<"neutral_face">> => <<"😐"/utf8>>,
      <<"expressionless">> => <<"😑"/utf8>>, <<"no_mouth">> => <<"😶"/utf8>>,
      <<"smirk">> => <<"😏"/utf8>>, <<"unamused">> => <<"😒"/utf8>>,
      <<"face_with_rolling_eyes">> => <<"🙄"/utf8>>,
      <<"grimacing">> => <<"😬"/utf8>>, <<"relieved">> => <<"😌"/utf8>>,
      <<"pensive">> => <<"😔"/utf8>>, <<"sleepy">> => <<"😪"/utf8>>,
      <<"sleeping">> => <<"😴"/utf8>>, <<"mask">> => <<"😷"/utf8>>,
      <<"nerd_face">> => <<"🤓"/utf8>>, <<"sunglasses">> => <<"😎"/utf8>>,
      <<"partying_face">> => <<"🥳"/utf8>>, <<"confused">> => <<"😕"/utf8>>,
      <<"worried">> => <<"😟"/utf8>>,
      <<"slightly_frowning_face">> => <<"🙁"/utf8>>,
      <<"open_mouth">> => <<"😮"/utf8>>, <<"hushed">> => <<"😯"/utf8>>,
      <<"astonished">> => <<"😲"/utf8>>, <<"flushed">> => <<"😳"/utf8>>,
      <<"pleading_face">> => <<"🥺"/utf8>>, <<"scream">> => <<"😱"/utf8>>,
      <<"cry">> => <<"😢"/utf8>>, <<"sob">> => <<"😭"/utf8>>,
      <<"disappointed">> => <<"😞"/utf8>>, <<"sweat">> => <<"😓"/utf8>>,
      <<"weary">> => <<"😩"/utf8>>, <<"tired_face">> => <<"😫"/utf8>>,
      <<"angry">> => <<"😠"/utf8>>, <<"rage">> => <<"😡"/utf8>>,
      <<"exploding_head">> => <<"🤯"/utf8>>,
      <<"melting_face">> => <<"🫠"/utf8>>,
      <<"saluting_face">> => <<"🫡"/utf8>>,
      <<"face_palm">> => <<"🤦"/utf8>>, <<"facepalm">> => <<"🤦"/utf8>>,
      <<"shrug">> => <<"🤷"/utf8>>, <<"raising_hand">> => <<"🙋"/utf8>>,
      <<"see_no_evil">> => <<"🙈"/utf8>>, <<"hear_no_evil">> => <<"🙉"/utf8>>,
      <<"speak_no_evil">> => <<"🙊"/utf8>>, <<"ghost">> => <<"👻"/utf8>>,
      <<"skull">> => <<"💀"/utf8>>, <<"robot_face">> => <<"🤖"/utf8>>,
      <<"poop">> => <<"💩"/utf8>>, <<"hankey">> => <<"💩"/utf8>>,
      <<"+1">> => <<"👍"/utf8>>, <<"thumbsup">> => <<"👍"/utf8>>,
      <<"-1">> => <<"👎"/utf8>>, <<"thumbsdown">> => <<"👎"/utf8>>,
      <<"ok_hand">> => <<"👌"/utf8>>, <<"wave">> => <<"👋"/utf8>>,
      <<"clap">> => <<"👏"/utf8>>, <<"raised_hands">> => <<"🙌"/utf8>>,
      <<"pray">> => <<"🙏"/utf8>>, <<"muscle">> => <<"💪"/utf8>>,
      <<"handshake">> => <<"🤝"/utf8>>, <<"crossed_fingers">> => <<"🤞"/utf8>>,
      <<"v">> => <<"✌️"/utf8>>, <<"point_up">> => <<"☝️"/utf8>>,
      <<"point_right">> => <<"👉"/utf8>>, <<"point_left">> => <<"👈"/utf8>>,
      <<"point_down">> => <<"👇"/utf8>>, <<"raised_hand">> => <<"✋"/utf8>>,
      <<"hand">> => <<"✋"/utf8>>, <<"fist">> => <<"✊"/utf8>>,
      <<"facepunch">> => <<"👊"/utf8>>, <<"punch">> => <<"👊"/utf8>>,
      <<"writing_hand">> => <<"✍️"/utf8>>, <<"eyes">> => <<"👀"/utf8>>,
      <<"brain">> => <<"🧠"/utf8>>, <<"heart">> => <<"❤️"/utf8>>,
      <<"orange_heart">> => <<"🧡"/utf8>>, <<"yellow_heart">> => <<"💛"/utf8>>,
      <<"green_heart">> => <<"💚"/utf8>>, <<"blue_heart">> => <<"💙"/utf8>>,
      <<"purple_heart">> => <<"💜"/utf8>>, <<"black_heart">> => <<"🖤"/utf8>>,
      <<"white_heart">> => <<"🤍"/utf8>>, <<"broken_heart">> => <<"💔"/utf8>>,
      <<"sparkling_heart">> => <<"💖"/utf8>>, <<"fire">> => <<"🔥"/utf8>>,
      <<"tada">> => <<"🎉"/utf8>>, <<"confetti_ball">> => <<"🎊"/utf8>>,
      <<"sparkles">> => <<"✨"/utf8>>, <<"star">> => <<"⭐"/utf8>>,
      <<"star2">> => <<"🌟"/utf8>>, <<"boom">> => <<"💥"/utf8>>,
      <<"100">> => <<"💯"/utf8>>, <<"zzz">> => <<"💤"/utf8>>,
      <<"white_check_mark">> => <<"✅"/utf8>>,
      <<"heavy_check_mark">> => <<"✔️"/utf8>>, <<"x">> => <<"❌"/utf8>>,
      <<"warning">> => <<"⚠️"/utf8>>, <<"no_entry">> => <<"⛔"/utf8>>,
      <<"no_entry_sign">> => <<"🚫"/utf8>>, <<"question">> => <<"❓"/utf8>>,
      <<"grey_question">> => <<"❔"/utf8>>, <<"exclamation">> => <<"❗"/utf8>>,
      <<"heavy_exclamation_mark">> => <<"❗"/utf8>>,
      <<"information_source">> => <<"ℹ️"/utf8>>, <<"sos">> => <<"🆘"/utf8>>,
      <<"new">> => <<"🆕"/utf8>>, <<"ok">> => <<"🆗"/utf8>>,
      <<"cool">> => <<"🆒"/utf8>>, <<"free">> => <<"🆓"/utf8>>,
      <<"heavy_plus_sign">> => <<"➕"/utf8>>,
      <<"heavy_minus_sign">> => <<"➖"/utf8>>,
      <<"arrow_right">> => <<"➡️"/utf8>>, <<"arrow_left">> => <<"⬅️"/utf8>>,
      <<"arrow_up">> => <<"⬆️"/utf8>>, <<"arrow_down">> => <<"⬇️"/utf8>>,
      <<"recycle">> => <<"♻️"/utf8>>, <<"red_circle">> => <<"🔴"/utf8>>,
      <<"large_green_circle">> => <<"🟢"/utf8>>,
      <<"large_yellow_circle">> => <<"🟡"/utf8>>,
      <<"large_blue_circle">> => <<"🔵"/utf8>>,
      <<"white_circle">> => <<"⚪"/utf8>>, <<"black_circle">> => <<"⚫"/utf8>>,
      <<"bulb">> => <<"💡"/utf8>>, <<"memo">> => <<"📝"/utf8>>,
      <<"pencil">> => <<"📝"/utf8>>, <<"pushpin">> => <<"📌"/utf8>>,
      <<"round_pushpin">> => <<"📍"/utf8>>, <<"paperclip">> => <<"📎"/utf8>>,
      <<"link">> => <<"🔗"/utf8>>, <<"calendar">> => <<"📆"/utf8>>,
      <<"date">> => <<"📅"/utf8>>, <<"hourglass">> => <<"⌛"/utf8>>,
      <<"alarm_clock">> => <<"⏰"/utf8>>, <<"stopwatch">> => <<"⏱️"/utf8>>,
      <<"rocket">> => <<"🚀"/utf8>>, <<"bug">> => <<"🐛"/utf8>>,
      <<"wrench">> => <<"🔧"/utf8>>, <<"hammer">> => <<"🔨"/utf8>>,
      <<"gear">> => <<"⚙️"/utf8>>, <<"lock">> => <<"🔒"/utf8>>,
      <<"key">> => <<"🔑"/utf8>>, <<"email">> => <<"📧"/utf8>>,
      <<"phone">> => <<"☎️"/utf8>>, <<"computer">> => <<"💻"/utf8>>,
      <<"speech_balloon">> => <<"💬"/utf8>>,
      <<"thought_balloon">> => <<"💭"/utf8>>,
      <<"chart_with_upwards_trend">> => <<"📈"/utf8>>,
      <<"chart_with_downwards_trend">> => <<"📉"/utf8>>,
      <<"moneybag">> => <<"💰"/utf8>>,
      <<"money_with_wings">> => <<"💸"/utf8>>,
      <<"trophy">> => <<"🏆"/utf8>>, <<"medal">> => <<"🏅"/utf8>>,
      <<"gift">> => <<"🎁"/utf8>>, <<"balloon">> => <<"🎈"/utf8>>,
      <<"christmas_tree">> => <<"🎄"/utf8>>, <<"santa">> => <<"🎅"/utf8>>,
      <<"coffee">> => <<"☕"/utf8>>, <<"tea">> => <<"🍵"/utf8>>,
      <<"beer">> => <<"🍺"/utf8>>, <<"beers">> => <<"🍻"/utf8>>,
      <<"wine_glass">> => <<"🍷"/utf8>>, <<"pizza">> => <<"🍕"/utf8>>,
      <<"cake">> => <<"🍰"/utf8>>, <<"tomato">> => <<"🍅"/utf8>>,
      <<"sunny">> => <<"☀️"/utf8>>, <<"sun_with_face">> => <<"🌞"/utf8>>,
      <<"cloud">> => <<"☁️"/utf8>>, <<"umbrella">> => <<"☔"/utf8>>,
      <<"snowflake">> => <<"❄️"/utf8>>, <<"zap">> => <<"⚡"/utf8>>,
      <<"rainbow">> => <<"🌈"/utf8>>, <<"earth_africa">> => <<"🌍"/utf8>>,
      <<"house">> => <<"🏠"/utf8>>, <<"car">> => <<"🚗"/utf8>>,
      <<"airplane">> => <<"✈️"/utf8>>, <<"train">> => <<"🚆"/utf8>>,
      <<"bike">> => <<"🚲"/utf8>>, <<"dog">> => <<"🐶"/utf8>>,
      <<"cat">> => <<"🐱"/utf8>>, <<"unicorn_face">> => <<"🦄"/utf8>>,
      <<"skin-tone-2">> => <<"🏻"/utf8>>, <<"skin-tone-3">> => <<"🏼"/utf8>>,
      <<"skin-tone-4">> => <<"🏽"/utf8>>, <<"skin-tone-5">> => <<"🏾"/utf8>>,
      <<"skin-tone-6">> => <<"🏿"/utf8>>}.

mapStatus(<<"away">>, SrvStatus) ->
    case SrvStatus of
        "Available" -> "Away";
        "Away"      -> "Away";
        "Busy"      -> "Away";
        "Offline"   -> "Offline";
        _           -> SrvStatus
    end;
mapStatus(<<"active">>, SrvStatus) ->
    case SrvStatus of
        "Available" -> "Available";
        "Away"      -> "Available";
        "Busy"      -> "Busy";
        "Offline"   -> "Offline";
        _           -> SrvStatus
    end;
mapStatus(_Presence, SrvStatus) ->
    SrvStatus.

mapStatus("Available") -> <<"auto">>;
mapStatus("Busy")      -> <<"auto">>;
mapStatus("Away")      -> <<"away">>;
mapStatus("Offline")   -> <<"away">>;
mapStatus(_Status)     -> <<"auto">>.

addExternalUser(ETodoUser, Choice) ->
    {ExtUserDesc, ExtUser} = ePluginInterface:getPeerInfo(Choice),
    ePluginInterface:saveExternalUser(ETodoUser,
                                      unicode:characters_to_binary(ExtUserDesc),
                                      unicode:characters_to_binary(ExtUser)).

getUsers(State = #state{users = Users}) ->
    sortChoices([<<"Slack - ", (userName(Id, State))/binary,
                   "<!", Id/binary, ">">>
                 || {Id, User} <- maps:to_list(Users),
                    not maps:get(<<"deleted">>, User, false)]).

getChannels(#state{convs = Convs}) ->
    sortChoices([<<"Slack - ", Name/binary, "<#", Name/binary, ">">>
                 || Conv = #{<<"name">> := Name} <- maps:values(Convs),
                    not maps:get(<<"is_im">>,   Conv, false),
                    not maps:get(<<"is_mpim">>, Conv, false)]).

sortChoices(Choices) ->
    Sort = fun(X, Y) ->
                   string:lowercase(X) =< string:lowercase(Y)
           end,
    lists:sort(Sort, [unicode:characters_to_list(Choice, utf8)
                      || Choice <- Choices]).
