%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2016, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 09 February 2016 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------

-module(plugin_sap).

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
         eSetStatusUpdate/5,
         eSetWorkLogDate/4,
         eMenuEvent/6]).

-import(eHtml, [htmlTag/0,   htmlTag/1,   htmlTag/2,
                headTag/0,   headTag/1,   headTag/2,
                bodyTag/0,   bodyTag/1,   bodyTag/2,
                titleTag/1,  titleTag/2,
                styleTag/1,  styleTag/2,
                tableTag/0,  tableTag/1,  tableTag/2,
                divTag/0,    divTag/1,    divTag/2,
                spanTag/0,   spanTag/1,   spanTag/2,
                fontTag/0,   fontTag/1,   fontTag/2,
                pTag/0,      pTag/1,      pTag/2,
                bTag/0,      bTag/1,      bTag/2,
                tdTag/0,     tdTag/1,     tdTag/2,
                thTag/0,     thTag/1,     thTag/2,
                trTag/0,     trTag/1,     trTag/2,
                brTag/0,
                formTag/0,   formTag/1,   formTag/2,
                aTag/0,      aTag/1,      aTag/2,
                selectTag/1, selectTag/2, inputTag/1,
                metaTag/1,   imgTag/1]).

-import(ePluginInterface, [getWeekDay/1, incDate/2, toStr/1]).

getName() -> "SAP".

getDesc() -> "SAP time reporting integration.".

-include_lib("wx/include/wx.hrl").
-include_lib("eTodo/include/eTodo.hrl").

%% Defines how clipboard data should look
-define(sapFormat, "~p\t\t\t\t~p\t\t\t\t~p\t\t\t\t~p\t\t\t\t~p").

-record(state, {frame, conf, date = date(), sapFormat}).

%%--------------------------------------------------------------------
%% @doc
%% initalize plugin.
%% @spec init() -> State.
%% @end
%%--------------------------------------------------------------------
init([WX, Frame]) ->
    Config = ePluginInterface:readConfig(?MODULE),
    wx:set_env(WX),
    #state{frame = Frame, conf = Config, sapFormat = ?sapFormat}.

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
getMenu(undefined, State) ->
    {ok, [{1900, "Add WBS"},
          {1901, "Remove WBS"},
          divider,
          {1902, "Verify work log"},
          divider,
          {1905, "5 days to clipboard (one WBS)"},
          {1906, "5 days to clipboard (all WBS)"},
          {1907, "5 days to message window (all WBS)"}], State};
getMenu(_ETodo, State) ->
    {ok, [{1900, "Add WBS"},
          {1901, "Remove WBS"},
          divider,
          {1902, "Verify work log"},
          divider,
          {1903, "Assign WBS to task"},
          {1904, "Remove WBS from task"},
          divider,
          {1905, "5 days to clipboard (one WBS)"},
          {1906, "5 days to clipboard (all WBS)"},
          {1907, "5 days to message window (all WBS)"}], State}.

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
%% The user receives a system message.
%%
%% @spec eReceivedMsg(Dir, User, Users, Text, State) -> NewState
%% @end
%%--------------------------------------------------------------------
eReceivedMsg(_EScriptDir, _User, _Users, _Text, State) ->
    State.

%%--------------------------------------------------------------------
%% @doc
%% The user receives an alarm.
%%
%% @spec eReceivedAlarmMsg(Dir, Text, State) -> NewState
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
eSetWorkLogDate(_Dir, _User, Date, State) ->
    State#state{date = Date}.

%%--------------------------------------------------------------------
%% @doc
%% Called for right click menu
%%
%% @spec eMenuEvent(EScriptDir, User, MenuOption, ETodo, MenuText, State) ->
%%       NewState
%% @end
%%--------------------------------------------------------------------
eMenuEvent(_EScriptDir, _User, 1900, _ETodo, _MenuText, State) ->
    doCreateProject(State);
eMenuEvent(_EScriptDir, _User, 1901, _ETodo, _MenuText, State) ->
    doDeleteProject(State);
eMenuEvent(_EScriptDir, User, 1902, _ETodo, _MenuText, State) ->
    doVerifyWorkLog(User, State);
eMenuEvent(_EScriptDir, _User, 1903, ETodo, _MenuText, State) ->
    doAssignProject(ETodo, State);
eMenuEvent(_EScriptDir, _User, 1904, ETodo, _MenuText, State) ->
    doRemoveFromProject(ETodo, State);
eMenuEvent(_EScriptDir, User, 1905, _ETodo, _MenuText, State) ->
    doCopyWeekToClipboardOneWBS(User, State);
eMenuEvent(_EScriptDir, User, 1906, _ETodo, _MenuText, State) ->
    doCopyWeekToClipboardAllWBS(User, State);
eMenuEvent(_EScriptDir, User, 1907, _ETodo, _MenuText, State) ->
    doCopyWeekToTableAllWBS(User, State);
eMenuEvent(_EScriptDir, _User, _MenuOption, _ETodo, _MenuText, State) ->
    State.

doCreateProject(State = #state{frame = Frame, conf = Config}) ->
    ProjDlg = wxTextEntryDialog:new(Frame, "Add WBS"),
    Config2 = case wxTextEntryDialog:showModal(ProjDlg) of
                  ?wxID_OK ->
                      PTxt  = toKey(wxTextEntryDialog:getValue(ProjDlg)),
                      Config#{PTxt => []};
                  _ ->
                      Config
              end,
    wxTextEntryDialog:destroy(ProjDlg),
    writeConfigChanges(Config, Config2),
    State#state{conf = Config2}.

doDeleteProject(State = #state{frame = Frame, conf = Config}) ->
    ProjDlg = wxSingleChoiceDialog:new(Frame, "Choose WBS to delete",
                                       "Delete WBS", map2GUI(Config)),
    wxSingleChoiceDialog:setSize(ProjDlg, {400, 400}),
    Config2 = case wxSingleChoiceDialog:showModal(ProjDlg) of
                  ?wxID_OK ->
                      PTxt = toKey(wxSingleChoiceDialog:getStringSelection(ProjDlg)),
                      maps:remove(PTxt, Config);
                  _ ->
                      Config
              end,
    wxSingleChoiceDialog:destroy(ProjDlg),
    writeConfigChanges(Config, Config2),
    State#state{conf = Config2}.

doVerifyWorkLog(User, State = #state{frame = Frame, date = Date, conf = Cfg}) ->
    WLList    = ePluginInterface:getWorkLog(User, Date),
    Keys      = maps:keys(Cfg),
    WLVerList = [{verify(Keys, Uid, Cfg), GUIDesc} || {Uid, GUIDesc} <- WLList],
    case constructVerifyMessage(WLVerList) of
        [] ->
            Dlg = wxMessageDialog:new(Frame, "All tasks in work log has WBS",
                                      [{caption, "Work log verified"},
                                       {style,   ?wxICON_INFORMATION}]),
            wxMessageDialog:showModal(Dlg),
            wxMessageDialog:destroy(Dlg);
        Msg ->
            Dlg = wxMessageDialog:new(Frame, Msg,
                                      [{caption, "Tasks missing WBS"},
                                       {style,   ?wxICON_INFORMATION}]),
            wxMessageDialog:showModal(Dlg),
            wxMessageDialog:destroy(Dlg)
    end,
    State.

constructVerifyMessage(VLVerList) ->
    constructVerifyMessage(VLVerList, []).

constructVerifyMessage([], Acc) ->
    lists:flatten(lists:reverse(Acc));
constructVerifyMessage([{false, GUIDesc}|Rest], Acc) ->
    constructVerifyMessage(Rest, [GUIDesc ++ "\n"|Acc]);
constructVerifyMessage([{true, _GUIDesc}|Rest], Acc) ->
    constructVerifyMessage(Rest, Acc).

verify([], _Uid, _Cfg) ->
    false;
verify([Key|Rest], Uid, Cfg) ->
    case lists:member(Uid, maps:get(Key, Cfg, [])) of
        true ->
            true;
        false ->
            verify(Rest, Uid, Cfg)
    end.

doAssignProject(ETodo, State = #state{frame = Frame, conf = Config}) ->
    ProjDlg = wxSingleChoiceDialog:new(Frame, "Assign WBS to task",
                                       "Assign WBS", map2GUI(Config)),
    wxSingleChoiceDialog:setSize(ProjDlg, {400, 400}),
    Config2 = case wxSingleChoiceDialog:showModal(ProjDlg) of
                  ?wxID_OK ->
                      Uid  = ETodo#etodo.uid,
                      PTxt = toKey(wxSingleChoiceDialog:getStringSelection(ProjDlg)),
                      Projects  = maps:get(PTxt, Config),
                      Projects2 = [Uid|lists:delete(Uid, Projects)],
                      Config#{PTxt := Projects2};
                  _ ->
                      Config
              end,
    wxSingleChoiceDialog:destroy(ProjDlg),
    writeConfigChanges(Config, Config2),
    State#state{conf = Config2}.

doRemoveFromProject(ETodo, State = #state{frame = Frame, conf = Config}) ->
    Choices = filterProjects(Config, ETodo#etodo.uid),
    ProjDlg = wxSingleChoiceDialog:new(Frame, "Remove WBS from task",
                                       "Remove WBS", Choices),
    wxSingleChoiceDialog:setSize(ProjDlg, {400, 400}),
    Config2 = case wxSingleChoiceDialog:showModal(ProjDlg) of
                  ?wxID_OK ->
                      Uid  = ETodo#etodo.uid,
                      PTxt = toKey(wxSingleChoiceDialog:getStringSelection(ProjDlg)),
                      Projects  = maps:get(PTxt, Config),
                      Projects2 = lists:delete(Uid, Projects),
                      Config#{PTxt := Projects2};
                  _ ->
                      Config
              end,
    wxSingleChoiceDialog:destroy(ProjDlg),
    writeConfigChanges(Config, Config2),
    State#state{conf = Config2}.

doCopyWeekToTableAllWBS(User, State = #state{conf  = Config, date = Date}) ->
    WBSList = map2GUI(Config),
    Html =
        trTag([{bgcolor, "black"}],
              [tdTag([{bgcolor, "white"}, {width, "5%"}], []),
               tdTag(fontTag([{color, "white"}], "WBS")),
               tdTag([{align, "center"}],
                     fontTag([{color, "white"}], getWeekDay(Date))),
               tdTag([{align, "center"}],
                     fontTag([{color, "white"}], getWeekDay(incDate(Date, 1)))),
               tdTag([{align, "center"}],
                     fontTag([{color, "white"}], getWeekDay(incDate(Date, 2)))),
               tdTag([{align, "center"}],
                     fontTag([{color, "white"}], getWeekDay(incDate(Date, 3)))),
               tdTag([{align, "center"}],
                     fontTag([{color, "white"}], getWeekDay(incDate(Date, 4)))),
               tdTag([{bgcolor, "white"}, {width, "5%"}], [])]),

    doCopyWeekToTableAllWBS(WBSList, User, Html, {0, 0, 0, 0, 0}, 0, State).

doCopyWeekToTableAllWBS([], _User, Html, {S1, S2, S3, S4, S5}, _C, State) ->
    HtmlRow =
        trTag([{bgcolor, "black"}],
              [tdTag([{bgcolor, "white"}, {width, "5%"}], []),
               tdTag([{width, "30%"}, {align, "left"}],
                     fontTag([{color, "white"}], "Total")),
               tdTag([{width, "12%"}, {align, "center"}],
                     fontTag([{color, "white"}], toString(S1))),
               tdTag([{width, "12%"}, {align, "center"}],
                     fontTag([{color, "white"}], toString(S2))),
               tdTag([{width, "12%"}, {align, "center"}],
                     fontTag([{color, "white"}], toString(S3))),
               tdTag([{width, "12%"}, {align, "center"}],
                     fontTag([{color, "white"}], toString(S4))),
               tdTag([{width, "12%"}, {align, "center"}],
                     fontTag([{color, "white"}], toString(S5))),
               tdTag([{bgcolor, "white"}, {width, "5%"}], [])]),

    EmptyRow = trTag([{bgcolor, "white"}], [tdTag([{colspan, 8}], [])]),

    ePluginInterface:systemEntry(""),
    Html2    = tableTag([Html, HtmlRow, EmptyRow, EmptyRow]),
    HtmlPage = unicode:characters_to_list(Html2, utf8),
    ePluginInterface:appendToPage(HtmlPage),
    State;
doCopyWeekToTableAllWBS([WBS|Rest], User, Html, Tot = {T1, T2, T3, T4, T5}, C,
                        State = #state{conf = Config, date  = Date}) ->
    PTxt    = toKey(WBS),
    Tasks   = maps:get(PTxt, Config),

    case calcWorkLog(User, Tasks, Date) of
        {0, 0, 0, 0, 0} ->
            doCopyWeekToTableAllWBS(Rest, User, Html, Tot, C, State);
        {D1, D2, D3, D4, D5} ->
            Color = if ((C rem 2) == 1) -> "#C0C0C0"; true -> "white" end,

            HtmlRow =
                trTag([{bgcolor, Color}],
                      [tdTag([{bgcolor, "white"}, {width, "10%"}], []),
                       tdTag([{width, "30%"}, {align, "left"}], WBS),
                       tdTag([{width, "12%"}, {align, "center"}], toString(D1)),
                       tdTag([{width, "12%"}, {align, "center"}], toString(D2)),
                       tdTag([{width, "12%"}, {align, "center"}], toString(D3)),
                       tdTag([{width, "12%"}, {align, "center"}], toString(D4)),
                       tdTag([{width, "12%"}, {align, "center"}], toString(D5)),
                       tdTag([{bgcolor, "white"}, {width, "5%"}], [])]),

            doCopyWeekToTableAllWBS(Rest, User, [Html, HtmlRow],
                                    {T1 + D1, T2 + D2, T3 + D3,
                                     T4 + D4, T5 + D5}, C + 1, State)
    end.

doCopyWeekToClipboardAllWBS(User, State = #state{conf  = Config}) ->
    WBSList = map2GUI(Config),
    doCopyWeekToClipboardAllWBS(WBSList, User, State).

doCopyWeekToClipboardAllWBS([], _User, State) ->
    State;
doCopyWeekToClipboardAllWBS([WBS|Rest], User,
                            State = #state{frame = Frame, conf      = Config,
                                           date  = Date,  sapFormat = SAPFormat}) ->

    PTxt    = toKey(WBS),
    Tasks   = maps:get(PTxt, Config),
    ExtDate = toStr(Date),

    case calcWorkLog(User, Tasks, Date) of
        {0, 0, 0, 0, 0} ->
            doCopyWeekToClipboardAllWBS(Rest, User, State);
        {D1, D2, D3, D4, D5} ->
            MsgDlg = wxMessageDialog:new(Frame, "5 days begining with " ++
                                             ExtDate ++ " in clipboard",
                                         [{caption, WBS},
                                          {style, ?wxICON_INFORMATION}]),
            CopyStr  = lists:flatten(io_lib:format(SAPFormat,
                                                   [D1, D2, D3, D4, D5])),
            CopyStr2 = replaceComma(CopyStr),
            ePluginInterface:toClipboard(CopyStr2),
            wxMessageDialog:showModal(MsgDlg),
            wxMessageDialog:destroy(MsgDlg),
            doCopyWeekToClipboardAllWBS(Rest, User, State)
    end.

doCopyWeekToClipboardOneWBS(User,
                            State = #state{frame = Frame, conf      = Config,
                                           date  = Date,  sapFormat = SAPFormat}) ->
    ExtDate = toStr(Date),
    ProjDlg = wxSingleChoiceDialog:new(Frame,
                                       "5 days begining with " ++ ExtDate ++
                                           " to clipboard",
                                       "Choose WBS", map2GUI(Config)),
    wxSingleChoiceDialog:setSize(ProjDlg, {400, 400}),
    case wxSingleChoiceDialog:showModal(ProjDlg) of
        ?wxID_OK ->
            PTxt  = toKey(wxSingleChoiceDialog:getStringSelection(ProjDlg)),
            Tasks = maps:get(PTxt, Config),
            {D1, D2, D3, D4, D5} = calcWorkLog(User, Tasks, Date),
            CopyStr = lists:flatten(io_lib:format(SAPFormat,
                                                  [D1, D2, D3, D4, D5])),
            CopyStr2 = replaceComma(CopyStr),
            ePluginInterface:toClipboard(CopyStr2);
        _ ->
            ok
    end,
    wxSingleChoiceDialog:destroy(ProjDlg),
    State.

filterProjects(Config, Uid) ->
    Projects = maps:keys(Config),
    filterProjects(Projects, Config, Uid, []).

filterProjects([], _, _, Acc) ->
    lists:reverse(Acc);
filterProjects([Project|Rest], Config, Uid, Acc) ->
    case lists:member(Uid, maps:get(Project, Config)) of
        true ->
            filterProjects(Rest, Config, Uid, [toGUI(Project)|Acc]);
        false ->
            filterProjects(Rest, Config, Uid, Acc)
    end.

calcWorkLog(User, Tasks, Date) ->
    calcWorkLog(User, Tasks, Date, {0, 0, 0, 0, 0}).

calcWorkLog(_User, [], _Date, Acc) ->
    Acc;
calcWorkLog(User, [Uid|Rest], Date, {D1, D2, D3, D4, D5}) ->
    {_, H1, M1} = ePluginInterface:getLoggedWork(User, Uid, Date),
    {_, H2, M2} = ePluginInterface:getLoggedWork(User, Uid, incDate(Date, 1)),
    {_, H3, M3} = ePluginInterface:getLoggedWork(User, Uid, incDate(Date, 2)),
    {_, H4, M4} = ePluginInterface:getLoggedWork(User, Uid, incDate(Date, 3)),
    {_, H5, M5} = ePluginInterface:getLoggedWork(User, Uid, incDate(Date, 4)),

    ND1 = addMinutes(D1 + H1, M1),
    ND2 = addMinutes(D2 + H2, M2),
    ND3 = addMinutes(D3 + H3, M3),
    ND4 = addMinutes(D4 + H4, M4),
    ND5 = addMinutes(D5 + H5, M5),

    calcWorkLog(User, Rest, Date, {ND1, ND2, ND3, ND4, ND5}).

addMinutes(Hours, Min) ->
    case Min rem 60 of
        0 ->
            Hours + Min div 60;
        _ ->
            Hours + Min/60
    end.

replaceComma(String) ->
    replaceComma(String, []).

replaceComma([], Acc) ->
    lists:reverse(Acc);
replaceComma([$.|Rest], Acc) ->
    replaceComma(Rest, [$,|Acc]);
replaceComma([Char|Rest], Acc) ->
    replaceComma(Rest, [Char|Acc]).

writeConfigChanges(Config, Config) ->
    ok;
writeConfigChanges(_Config, Config2) ->
    ePluginInterface:saveConfig(?MODULE, Config2).

toKey(Value) ->
    base64:encode_to_string(unicode:characters_to_binary(Value)).

toGUI(Value) ->
    unicode:characters_to_list(base64:decode(Value), utf8).

map2GUI(Map) ->
    lists:sort([toGUI(Value) || Value <- maps:keys(Map)]).


toString(0) ->
    spanTag([{style, "color:grey;"}], "00:00");
toString(Number) when is_integer(Number) and (Number < 10) ->
    "0" ++ integer_to_list(Number) ++ ":00";
toString(Number) when is_integer(Number) ->
    integer_to_list(Number) ++ ":00";
toString(Number) ->
    Minutes = trunc((Number - trunc(Number)) * 60),
    Hours   = trunc(Number),
    case {Hours < 10, Minutes < 10} of
        {true, true} ->
            "0" ++ integer_to_list(Hours) ++ ":0" ++ integer_to_list(Minutes);
        {false, true}->
            integer_to_list(Hours) ++ ":0" ++ integer_to_list(Minutes);
        {true, false}->
            "0" ++ integer_to_list(Hours) ++ ":" ++ integer_to_list(Minutes);
        {false, false} ->
            integer_to_list(Hours) ++ ":" ++ integer_to_list(Minutes)
    end.
