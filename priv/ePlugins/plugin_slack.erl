%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2013, Mikael Bylund
%%% @doc
%%%
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

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {frame,
                slackUrl, status, statusText, srvStatus,
                slackToken, slackBotToken,
                slackUsers, slackChannels, userProfile,
                slackConn, slackRef,
                wsCon, wsReconnectUrl}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init([WX, Frame]) ->
    DefaultUrl     = "https://slack.com/api",

    Url    = application:get_env(eTodo, slackUrl,      DefaultUrl),
    Token  = application:get_env(eTodo, slackToken,    ""),
    BToken = application:get_env(eTodo, slackBotToken, ""),

    wx:set_env(WX),

    application:ensure_all_started(gun),
    {ConnPid, Ref} = startSetupWebsocket(Url, BToken),

    #state{frame         = Frame,
           srvStatus     = "Available",
           slackUrl      = Url,
           slackToken    = Token,
           slackBotToken = BToken,
           slackConn     = ConnPid,
           slackRef      = {"rtm.connect", Ref, <<>>}}.

startSetupWebsocket(Url, Token) ->
    {Server, Port, _} = getServerAndPort(Url),
    {ok, ConnPid}     = gun:open(Server, Port),
    {ok, _Protocol}   = gun:await_up(ConnPid),
    BinToken = list_to_binary(Token),
    Body     = <<"token=", BinToken/binary>>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    Ref = gun:post(ConnPid, "/api/rtm.connect", Headers, Body),
    {ConnPid, Ref}.

getServerAndPort(Url) when is_binary(Url) ->
    getServerAndPort(binary_to_list(Url));
getServerAndPort(Url) ->
    SchemeDefaults = http_uri:scheme_defaults() ++ [{wss,  443}, {ws, 80}],
    case http_uri:parse(Url, [{scheme_defaults, SchemeDefaults}]) of
        {ok,{_Scheme,[], Server, Port, Path,[]}} ->
            {Server, Port, Path};
        _ ->
            {Url, 443, ""}
    end.

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
getMenu(_ETodo, State) -> {ok, [], State}.

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
    {ok, default(State#state.status, Status),
     default(State#state.statusText, StatusMsg),
     State#state{status = undefined, statusText = undefined}}.

default(undefined, Value) when is_binary(Value) -> binary_to_list(Value);
default(Value, _Default)  when is_binary(Value) -> binary_to_list(Value);
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
eTimerStarted(_Dir, _User, _Text, _Hours, _Min, _Sec, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user stopped timer
%%
%% @spec eTimerStopped(EScriptDir, User, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerStopped(_EScriptDir, _User, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Timer ended
%%
%% @spec eTimerEnded(EScriptDir, User, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eTimerEnded(_EScriptDir, _User, _Text, State) ->
    State.

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
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
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
eSendMsg(_EScriptDir, User, Users, Text, State) ->
    postChatMessages(State, User, Users, Text),
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes his/her status in eTodo
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eSetStatusUpdate(_Dir, _User, Status, StatusMsg, State) ->
    SlackRef = setUserProfile(State#state.slackConn,
                              State#state.slackToken, StatusMsg),
    setUserPresence(State#state.slackConn,
                    State#state.slackToken, mapStatus(Status)),
    State#state{srvStatus = Status, slackRef = SlackRef}.

%%--------------------------------------------------------------------
%% @doc
%% Called every time the user changes work log date
%% @spec eSetWorkLogDate(Dir, User, Date, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eSetWorkLogDate(_Dir, _User, _Date, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Called for right click menu
%%
%% @spec eMenuEvent(EScriptDir, User, MenuOption, ETodo, MenuText, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, _User, _MenuOption, _ETodo, _MenuText, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% Handle messages sent to this plugin
%%
%% @spec handleInfo(Info, State) -> NewState
%% @end
%%--------------------------------------------------------------------
handleInfo({gun_data, _Pid, Ref, nofin, Body},
    State = #state{slackRef = {Command, Ref, SoFar}}) ->
    State#state{slackRef = {Command, Ref, <<SoFar/binary, Body/binary>>}};
handleInfo({gun_data, _Pid, Ref, fin, Body},
           State = #state{slackRef = {"rtm.connect", Ref, SoFar}}) ->
    JSON  = jsx:decode(<<SoFar/binary, Body/binary>>, [return_maps]),
    WSUrl = maps:get(<<"url">>, JSON),
    {Server, Port, Path}  = getServerAndPort(WSUrl),
    {ok, ConnPid}   = gun:open(Server, Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, Path),
    NewRef = getUserInfo(State#state.slackConn, State#state.slackToken),
    State#state{wsCon = ConnPid, slackRef = NewRef};
handleInfo({gun_data, Pid, Ref, fin, Body},
           State = #state{slackRef = {"users.list", Ref, SoFar}}) ->
    JSON  = jsx:decode(<<SoFar/binary, Body/binary>>, [return_maps]),
    Users = maps:get(<<"members">>, JSON),
    NewRef = getChannelInfo(Pid, State#state.slackToken),
    State#state{slackRef = NewRef, slackUsers = Users};
handleInfo({gun_data, Pid, Ref, fin, Body},
           State = #state{slackRef = {"channels.list", Ref, SoFar}}) ->
    JSON     = jsx:decode(<<SoFar/binary, Body/binary>>, [return_maps]),
    Channels = maps:get(<<"channels">>, JSON),
    NewRef   = getUserProfile(Pid, State#state.slackToken),
    State#state{slackRef = NewRef, slackChannels = Channels};
handleInfo({gun_data, Pid, Ref, fin, Body},
           State = #state{slackRef = {"users.profile.get", Ref, SoFar}}) ->
    JSON        = jsx:decode(<<SoFar/binary, Body/binary>>, [return_maps]),
    UserProfile = maps:get(<<"profile">>, JSON),
    State#state{slackRef = undefined, userProfile = UserProfile};
handleInfo({gun_ws, _Pid, {text, Body}}, State) ->
    JSON = jsx:decode(Body, [return_maps]),
    Type = maps:get(<<"type">>, JSON),
    case {Type, JSON} of
        {<<"reconnect_url">>, #{<<"url">> := Url}} ->
            State#state{wsReconnectUrl = Url};
        {<<"message">>, #{<<"user">>    := User,
                          <<"text">>    := Text,
                          <<"channel">> := Channel}} ->
            BotId = maps:get(<<"bot_id">>, JSON, <<>>),
            handleMsg(State, BotId, User, Text, Channel);
        {<<"user_typing">>, _} ->
            setWriting(State);
        {<<"presence_change">>, #{<<"user">> := User}} ->
            Presence = maps:get(<<"presence">>, JSON, <<"active">>),
            setPresence(State, User, Presence);
        {<<"user_change">>, JSON} ->
            UserMap   = maps:get(<<"user">>,        JSON,    #{}),
            User      = maps:get(<<"id">>,          UserMap, <<>>),
            Profile   = maps:get(<<"profile">>,     UserMap, #{}),
            StatusTxt = maps:get(<<"status_text">>, Profile, <<>>),
            setStatusText(State, User, StatusTxt);
        _ ->
            io:format("Received message on websocket: ~p~n", [JSON]),
            State
    end;
handleInfo({gun_up, Pid, http2},
           State = #state{wsReconnectUrl = undefined, wsCon = Pid}) ->
    Token    = State#state.slackBotToken,
    BinToken = list_to_binary(Token),
    Body     = <<"token=", BinToken/binary>>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    Ref = gun:post(Pid, "/api/rtm.connect", Headers, Body),
    State#state{slackRef = Ref};
handleInfo({gun_up, Pid, http2},
            State = #state{wsReconnectUrl = WSUrl, wsCon = Pid}) ->
    {_Server, _Port, Path}  = getServerAndPort(WSUrl),
    gun:ws_upgrade(Pid, Path),
    State;
handleInfo(_Info, State) ->
    %% io:format("~p~n", [Info]),
    State.

postChatMessages(State, User, Users, Text) ->
    UserList = string:tokens(Users, ";"),
    UserCfg  = eTodoDB:readUserCfg(User),
    OwnerCfg = UserCfg#userCfg.ownerCfg,
    [postChatMessage(State, OwnerCfg, EUser, Text) || EUser <- UserList].

postChatMessage(_State, [], _User, _Text) ->
    ok;
postChatMessage(State, [Owner|Rest], User, Text) ->
    case eTodoUtils:getPeerInfo(Owner) of
        {User, "!" ++ SlackChannel} ->
            postChatMessage(State, SlackChannel, Text);
        {User, "#" ++ SlackChannel} ->
            postChatMessage(State, "#" ++ SlackChannel, Text);
        _ ->
            postChatMessage(State, Rest, User, Text)
    end.

postChatMessage(State, SlackChannel, Text) ->
    JSON  = jsx:encode(#{channel => unicode:characters_to_binary(SlackChannel),
                         token   => list_to_binary(State#state.slackToken),
                         text    => unicode:characters_to_binary(Text),
                         as_user => true}),
    Token   = State#state.slackToken,
    Headers = createHeaders(Token, JSON, "application/json; charset=utf-8"),
    gun:post(State#state.slackConn, "/api/chat.postMessage", Headers, JSON).

createHeaders(Token, Body, ContentType) ->
    [{<<"content-type">>,   ContentType},
     {<<"content-length">>, byte_size(Body)},
     {<<"authorization">>,  "Bearer " ++ Token}].

getUserInfo(Connection, Token) ->
    BinToken = list_to_binary(Token),
    Body     = <<"token=", BinToken/binary, "&presence=false">>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    Ref      = gun:post(Connection, "/api/users.list", Headers, Body),
    {"users.list", Ref, <<>>}.

getChannelInfo(Connection, Token) ->
    BinToken = list_to_binary(Token),
    Body     = <<"token=", BinToken/binary, "&exclude_archived=true">>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    Ref      = gun:post(Connection, "/api/channels.list", Headers, Body),
    {"channels.list", Ref, <<>>}.

getUserProfile(Connection, Token) ->
    BinToken = list_to_binary(Token),
    Body     = <<"token=", BinToken/binary>>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    Ref      = gun:post(Connection, "/api/users.profile.get", Headers, Body),
    {"users.profile.get", Ref, <<>>}.

setUserProfile(Connection, Token, "") ->
    BinToken = list_to_binary(Token),
    Profile  = cow_uri:urlencode(jsx:encode(#{status_text  => <<>>,
                                              status_emoji => <<>>})),
    Body     = <<"token=", BinToken/binary, "&profile=", Profile/binary>>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    gun:post(Connection, "/api/users.profile.set", Headers, Body);
setUserProfile(Connection, Token, StatusText) ->
    BinToken = list_to_binary(Token),
    StatTxt2 = unicode:characters_to_binary(StatusText),
    Profile  = cow_uri:urlencode(jsx:encode(#{status_text => StatTxt2})),
    Body     = <<"token=", BinToken/binary, "&profile=", Profile/binary>>,
    Headers  = createHeaders(Token, Body, "application/x-www-form-urlencoded"),
    gun:post(Connection, "/api/users.profile.set", Headers, Body).

setUserPresence(Connection, Token, Status) ->
    BinToken = list_to_binary(Token),
    Body     = jsx:encode(#{token    => BinToken,
                            presence => Status}),
    Headers  = createHeaders(Token, Body, "application/json; charset=utf-8"),
    gun:post(Connection, "/api/users.setPresence", Headers, Body).

handleMsg(State, <<>>, User, Text, Channel) ->
    sendMsg(User, State, Channel, Text);
handleMsg(State, _BotId, User, Text, Channel) ->
    case myUser(User, State) of
        true ->
            State;
        false ->
            sendMsg(User, State, Channel, Text)
    end.

sendMsg(User, State, Channel, Text) ->
    UserName       = getUserName(User, State),
    ChannelName    = getChannelName(Channel, State),
    ChannelGUIDesc = getGUIDesc(ChannelName),
    ePluginInterface:msgEntry(UserName, [ChannelGUIDesc], Text),
    State.

setWriting(State) ->
    State.

setPresence(State, User, Presence) ->
    case myUser(User, State) of
        true ->
            State#state{status = mapStatus(Presence, State#state.srvStatus)};
        false ->
            State
    end.

setStatusText(State, User, StatusTxt) ->
    case myUser(User, State) of
        true ->
            State#state{statusText = StatusTxt};
        false ->
            State
    end.

myUser(User, #state{slackUsers = SlackUsers, userProfile = Profile}) ->
    MyEmail = maps:get(<<"email">>, Profile, <<"my_email">>),
    myUser(User, SlackUsers, MyEmail).

myUser(_User, [], _MyEmail) ->
    false;
myUser(User, [SlackUser|Rest], MyEmail) ->
    case User == maps:get(<<"id">>, SlackUser, <<>>) of
        true ->
            Profile   = maps:get(<<"profile">>, SlackUser, <<>>),
            EmailAddr = maps:get(<<"email">>, Profile, <<"user_email">>),
            MyEmail == EmailAddr;
        false ->
            myUser(User, Rest, MyEmail)
    end.

getUserName(User, #state{slackUsers = SlackUsers}) ->
    doGetUserName(User, SlackUsers).

doGetUserName(User, []) ->
    User;
doGetUserName(User, [SlackUser|Rest]) ->
    case User == maps:get(<<"id">>, SlackUser, <<>>) of
        true ->
            savePortrait(User, SlackUser);
        false ->
            doGetUserName(User, Rest)
    end.

getChannelName(Channel, #state{slackChannels = SlackChannels}) ->
    doGetChannelName(Channel, SlackChannels).

doGetChannelName(Channel, []) ->
    Channel;
doGetChannelName(Channel, [SlackChannel|Rest]) ->
    case Channel == maps:get(<<"id">>, SlackChannel, <<>>) of
        true ->
            maps:get(<<"name">>, SlackChannel, Channel);
        false ->
            doGetChannelName(Channel, Rest)
    end.

getGUIDesc(ChannelName) ->
    case ePluginInterface:loggedIn() of
        false ->
            ChannelName;
        {true, User} ->
            UserCfg  = eTodoDB:readUserCfg(User),
            OwnerCfg = UserCfg#userCfg.ownerCfg,
            getGUIDescription(ChannelName, OwnerCfg)
    end.

getGUIDescription(ChannelName, undefined) ->
    ChannelName;
getGUIDescription(ChannelName, []) ->
    ChannelName;
getGUIDescription(ChannelName, [Owner|Rest]) ->
    ChannelDesc = binary_to_list(ChannelName),
    case eTodoUtils:getPeerInfo(Owner) of
        {User, "#" ++ ChannelDesc} ->
            list_to_binary(User);
        {User, "!" ++ ChannelDesc} ->
            list_to_binary(User);
        _ ->
            getGUIDescription(ChannelName, Rest)
    end.

savePortrait(_User, #{<<"profile">> := #{<<"image_72">>     := Url,
                                         <<"display_name">> := DisplayName}}) ->
    Opt = [{ssl, [{verify, verify_none}]}],
    case httpc:request(get, {binary_to_list(Url), ""}, Opt,
                       [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Picture}} ->
            ePluginInterface:setPortrait(binary_to_list(DisplayName),
                                         Picture, false);
        _ ->
            ok
    end,
    DisplayName;
savePortrait(User, _Other) ->
    User.


mapStatus(<<"away">>, SrvStatus) ->
    case SrvStatus of
        "Available" -> "Away";
        "Away"      -> "Away";
        "Busy"      -> "Away";
        "Offline"   -> "Offline"
    end;
mapStatus(<<"active">>, SrvStatus) ->
    case SrvStatus of
        "Available" -> "Available";
        "Away"      -> "Available";
        "Busy"      -> "Busy";
        "Offline"   -> "Offline"
    end.

mapStatus("Available") -> <<"auto">>;
mapStatus("Busy")      -> <<"auto">>;
mapStatus("Away")      -> <<"away">>;
mapStatus("Offline")   -> <<"away">>.