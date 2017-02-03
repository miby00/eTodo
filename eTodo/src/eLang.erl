%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2016, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 08. Jun 2016 11:56
%%%-------------------------------------------------------------------
-module(eLang).
-author("Mikael Bylund <mikael.bylund@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/1,
         tr/1,
         tr/2,
         setLanguage/1,
         getLanguage/0,
         initiateGUI/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(eGuiFunctions, [type/1, obj/2]).

-include_lib("eTodo/include/eTodo.hrl").


%% To add a new language construct a new hrl file with one of the ones below
%% as template. Add the new language to languages and then you are done.

-include("eLangSwedish.hrl").
-include("eLangEnglish.hrl").

-define(SERVER, ?MODULE).

-record(state, {language = english, guiState}).

-define(languages, [{swedish, {?swedishWX, ?swedishDef}},
                    {english, {?englishWX, ?englishDef}}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(WXEnv :: term()) ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(WXEnv) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WXEnv], []).

tr(LanguageString) ->
    gen_server:call(?MODULE, {translate, LanguageString}).

tr(Language, Text) ->
    {Dict1, Dict2} = proplists:get_value(Language, ?languages, #{}),
    maps:get(Text, Dict1, maps:get(Text, Dict2, Text)).

setLanguage(Language) ->
    case proplists:get_value(Language, ?languages) of
        undefined ->
            ok;
        _ ->
            gen_server:cast(?MODULE, {setLanguage, Language})
    end.

getLanguage() ->
    gen_server:call(?MODULE, getLanguage).

initiateGUI(GUIState) ->
    gen_server:cast(?MODULE, {initiateGUI, GUIState}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
             {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term()} | ignore).
init([WXEnv]) ->
    wx:set_env(WXEnv),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
             {reply, Reply :: term(), NewState :: #state{}} |
             {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_call({translate, LangString}, _From, State) ->
    Translation = tr(State#state.language, LangString),
    {reply, Translation, State};
handle_call(getLanguage, _From, State) ->
    {reply, State#state.language, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({setLanguage, Language}, State = #state{language = Language}) ->
    {noreply, State};
handle_cast({setLanguage, Language}, State) ->
    case proplists:get_value(Language, ?languages) of
        undefined ->
            {noreply, State};
        {Dict1, _Dict2} ->
            Components = maps:keys(Dict1),
            doInitiateGUI(Language, Components, State#state.guiState),
            {noreply, State#state{language = Language}}
    end;
handle_cast({initiateGUI, GUIState}, State = #state{language = Language}) ->
    {Dict1, _Dict2} = proplists:get_value(Language, ?languages),
    Components = maps:keys(Dict1),
    doInitiateGUI(Language, Components, GUIState),
    {noreply, State#state{guiState = GUIState}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}} |
             {noreply, NewState :: #state{}, timeout() | hibernate} |
             {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

doInitiateGUI(_Language, _Components, undefined) ->
    ok;
doInitiateGUI(_Language, [], _GuiState) ->
    ok;
doInitiateGUI(Language, Components, GUIState) ->
    [translate(Language, Component, GUIState) || Component <- Components],

    %% Make notebook update
    Notebook = obj("mainNotebook", GUIState),
    wxNotebook:setPageText(Notebook, 0, tr(Language, "mainPanel")),

    %% Make message menu update
    wxMenu:setLabel(GUIState#guiState.msgMenu, ?clearMsg, ?clearMsgText(Language)),
    wxMenu:setLabel(GUIState#guiState.msgMenu, ?clearRem, ?clearRemText(Language)).

translate(Language, Component, GuiState) ->
    translate(Language, Component, type(Component), GuiState).

translate(Language, Component, wxStaticText, GuiState) ->
    Object     = obj(Component, GuiState),
    Translated = tr(Language, Component),
    wxStaticText:setLabel(Object, Translated);
translate(Language, Component, wxCheckBox, GuiState) ->
    Object     = obj(Component, GuiState),
    Translated = tr(Language, Component),
    wxCheckBox:setLabel(Object, Translated);
translate(Language, Component, wxPanel, GuiState) ->
    Object     = obj(Component, GuiState),
    Translated = tr(Language, Component),
    wxPanel:setLabel(Object, Translated);
translate(Language, Component, wxMenuItem, GuiState) ->
    ID         = wxXmlResource:getXRCID(Component),
    Translated = tr(Language, Component),
    wxMenuBar:setLabel(GuiState#guiState.menuBar, ID, Translated);
translate(Language, Component, wxMenu, GuiState) ->
    Pos = case Component of
              "fileMenu" -> 0;
              "editMenu" -> 1;
              "helpMenu" -> 2
          end,
    Translated = tr(Language, Component),
    wxMenuBar:setLabelTop(GuiState#guiState.menuBar, Pos, Translated);
translate(_Language, _Component, _Type, _GuiState) ->
    ok.


