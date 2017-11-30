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
         eMenuEvent/6]).

getName() -> "eSlack".

getDesc() -> "An eTodo slack integration.".

-include_lib("eTodo/include/eTodo.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {frame, slackUrl, slackToken}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init([WX, Frame]) ->
    DefaultUrl     = "https://slack.com/api",

    Url   = application:get_env(eTodo, slackUrl,   DefaultUrl),
    Token = application:get_env(eTodo, slackToken, ""),

    wx:set_env(WX),
    #state{frame         = Frame,
           slackUrl      = Url,
           slackToken    = Token}.

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

postChatMessages(State, User, Users, Text) ->
    UserList = string:tokens(Users, ";"),
    UserCfg  = eTodoDB:readUserCfg(User),
    OwnerCfg = UserCfg#userCfg.ownerCfg,
    [postChatMessage(State, OwnerCfg, User, Text) || User <- UserList].

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
    Token = State#state.slackToken,
    Url   = State#state.slackUrl ++ "/chat.postMessage",
    JSON  = jsx:encode(#{token   => list_to_binary(Token),
                         channel => list_to_binary(SlackChannel),
                         as_user => true,
                         text    => unicode:characters_to_binary(Text)}),
    httpPost(Token, post, JSON, Url, State#state.frame).


httpPost(Token, Method, Body, Url, Frame) ->
    AuthOption  = {"Authorization", "Bearer " ++ Token},
    ContentType = {"Content-Type", "application/json; charset=utf-8"},
    Request     = {Url, [AuthOption, ContentType], "application/json", Body},
    Options     = [{body_format,binary}],
    case httpc:request(Method, Request, [{url_encode, false}], Options) of
        {ok, {{_HTTPVersion, Status, _Reason}, _Headers, RBody}}
          when (Status == 200) or (Status == 201) or (Status == 204) ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, RBody],
                     "http success", ?LINE),
            Body;
        {ok, {{_HTTPVersion, _Error, Reason}, _Headers, RBody}} ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, RBody],
                     "http failure", ?LINE),
            MsgDlg = wxMessageDialog:new(Frame, "Error occured: " ++ Reason,
                                         [{style,   ?wxICON_ERROR},
                                          {caption, "Http failure"}]),
            wxMessageDialog:showModal(MsgDlg),
            wxMessageDialog:destroy(MsgDlg),
            Body;
        Else ->
            eLog:log(debug, ?MODULE, init, [Method, Url, Body, Else],
                     "http failure", ?LINE),
            Else
    end.
