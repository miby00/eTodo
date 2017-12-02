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

getDesc() -> "An eTodo slack integration.".

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {frame, slackUrl, slackToken, slackBotToken,
                slackConn, slackRef, wsCon, wsReconnectUrl}).

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
           slackUrl      = Url,
           slackToken    = Token,
           slackBotToken = BToken,
           slackConn     = ConnPid,
           slackRef      = Ref}.

startSetupWebsocket(Url, Token) ->
    {Server, Port, _} = getServerAndPort(Url),
    {ok, ConnPid}     = gun:open(Server, Port),
    {ok, _Protocol}   = gun:await_up(ConnPid),
    BinToken = list_to_binary(Token),
    Body     = <<"token=", BinToken/binary>>,
    ContentType   = "application/x-www-form-urlencoded",
    ContentLength = byte_size(Body),
    Headers = [{<<"content-type">>,   ContentType},
               {<<"content-length">>, ContentLength}],
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
%% Called every 15 seconds to check if someone changes status
%% outside eTodo.
%% Status can be "Available" | "Away" | "Busy" | "Offline"
%% @spec eSetStatusUpdate(Dir, User, Status, StatusMsg, State) ->
%%       {ok, Status, StatusMsg, NewState}
%% @end
%%--------------------------------------------------------------------
eGetStatusUpdate(_Dir, _User, Status, StatusMsg, State) ->
    {ok, Status, StatusMsg, State}.

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
eSetStatusUpdate(_Dir, _User, _Status, _StatusMsg, State) ->
    State.

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
handleInfo({gun_data, _Pid, Ref, _, Body}, State = #state{slackRef = Ref}) ->
    JSON  = jsx:decode(Body, [return_maps]),
    WSUrl = maps:get(<<"url">>, JSON),
    {Server, Port, Path}  = getServerAndPort(WSUrl),
    {ok, ConnPid}   = gun:open(Server, Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, Path),
    State#state{wsCon = ConnPid, slackRef = undefined};
handleInfo({gun_ws, Pid, {text, Body}}, State) ->
    JSON = jsx:decode(Body, [return_maps]),
    Type = maps:get(<<"type">>, JSON),
    case Type of
        <<"reconnect_url">> ->
            Url = maps:get(<<"url">>, JSON),
            State#state{wsReconnectUrl = Url};
        _ ->
            io:format("Pid: ~p: Msg: ~p~n", [Pid, JSON]),
            State
    end;
handleInfo({gun_up, Pid, http2}, State = #state{wsReconnectUrl = WSUrl}) ->
    {_Server, _Port, Path}  = getServerAndPort(WSUrl),
    gun:ws_upgrade(Pid, Path),
    State;
handleInfo(Info, State) ->
    io:format("~p~n", [Info]),
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
    ContentType   = "application/json; charset=utf-8",
    ContentLength = byte_size(JSON),
    Headers = [{<<"content-type">>,   ContentType},
               {<<"content-length">>, ContentLength},
               {<<"authorization">>,  "Bearer " ++ State#state.slackToken}],
    gun:post(State#state.slackConn, "/api/chat.postMessage", Headers, JSON).