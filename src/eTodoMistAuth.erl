-module(eTodoMistAuth).

-behaviour(gen_server).

-export([start_link/0, login/2, external_login/1, session_user/1, logout/1, todo_data/3,
         set_done/4, create_list/3, delete_list/3, update_task/12, set_reminder/9, delete_task/3, create_task/11, settings_data/1,
         save_settings/6, status_form/1, set_status/4, send_message/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eTodo/include/eTodo.hrl").

-define(SESSION_LIFETIME, 28800).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(UserName, Password) ->
    gen_server:call(?MODULE, {login, UserName, Password}).

%% Session for a user already authenticated elsewhere (Google sign-in).
%% The user must be the local account or one of its peers.
external_login(UserName) ->
    gen_server:call(?MODULE, {external_login, UserName}).

session_user(Token) ->
    gen_server:call(?MODULE, {session_user, Token}).

logout(Token) ->
    gen_server:call(?MODULE, {logout, Token}).

todo_data(Token, ListName, ShowAll) ->
    gen_server:call(?MODULE, {todo_data, Token, ListName, ShowAll}).

settings_data(Token) ->
    gen_server:call(?MODULE, {settings_data, Token}).

status_form(Token) ->
    gen_server:call(?MODULE, {status_form, Token}).

%% Only the local account may change its status. Offline is not offered
%% since it would log the account out.
set_status(Token, Csrf, Status0, Message0) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            User = unicode:characters_to_list(UserName),
            Status = to_list(Status0),
            Message = clean_status_message(to_list(Message0)),
            case is_owner(User) andalso
                lists:member(Status, ["Available", "Busy", "Away"]) of
                true ->
                    eTodoStatus:change_status(User, Status, Message),
                    {ok, nil};
                false ->
                    {error, invalid_status}
            end;
        {error, _} = Error ->
            Error
    end.

%% Chat from the web GUI. Only the local account may send, and only to
%% peers that are online right now ("All" means every online peer).
send_message(Token, Csrf, To0, Text0) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            User  = unicode:characters_to_list(UserName),
            Text  = string:trim(unicode:characters_to_list(
                                  string:replace(to_list(Text0), "\r\n", "\n",
                                                 all))),
            Peers = eMessagesView:online_peers(User),
            Users = case to_list(To0) of
                        "All" -> Peers;
                        Peer  -> [P || P <- Peers, P =:= Peer]
                    end,
            case is_owner(User) andalso Users =/= [] andalso Text =/= ""
                andalso length(Text) =< 4000 of
                true ->
                    deliver_message(User, Users, Text),
                    {ok, nil};
                false ->
                    {error, invalid_message}
            end;
        {error, _} = Error ->
            Error
    end.

deliver_message(User, Users, Text) ->
    ePeerEM:sendMsg(User, Users, msgEntry, Text),
    ePluginServer:eSendMsg(User, Users, Text),
    case application:get_env(eTodo, mode) of
        {ok, "noGui"} ->
            {Html, _} = eHtml:generateMsg(User, User, Users, Text),
            eTodoDB:appendToPage(User, msgEntry, User, Users, Html);
        _ ->
            eTodo:webMsgSent(Users, Text)
    end.

clean_status_message(Message) ->
    Clean = [C || C <- string:trim(Message), C >= 32, C =/= 127],
    lists:sublist(Clean, 200).

save_settings(Token, Csrf, Status, Prio, Sort1, Sort2) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            save_settings(unicode:characters_to_list(UserName),
                          [{"filterStatus", to_list(Status)},
                           {"filterPrio", to_list(Prio)},
                           {"sortOrder", to_list(Sort1)},
                           {"sortOrderSec", to_list(Sort2)}]);
        {error, _} = Error ->
            Error
    end.

%% Only the local account may change tasks; peer logins are read-only.
set_done(Token, Csrf, Uid, Done) when is_boolean(Done) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            update_done(unicode:characters_to_list(UserName), Uid, Done);
        {error, _} = Error ->
            Error
    end.

%% Adds a todo list for the local account. An existing name is not an
%% error; the caller just selects it.
create_list(Token, Csrf, Name0) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            add_list(unicode:characters_to_list(UserName),
                     string:trim(to_list(Name0)));
        {error, _} = Error ->
            Error
    end.

%% Removes a list from the local account. Its tasks stay in "All tasks".
delete_list(Token, Csrf, Name0) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            remove_list(unicode:characters_to_list(UserName), to_list(Name0));
        {error, _} = Error ->
            Error
    end.

remove_list(UserName, Name) ->
    UserCfg = eTodoDB:readUserCfg(UserName),
    Current = case UserCfg#userCfg.lists of
                  undefined -> [];
                  Lists -> Lists
              end,
    Reserved = [?defTaskList, ?defInbox, ?defLoggedWork],
    case is_owner(UserName) andalso lists:member(Name, Current)
         andalso not lists:member(Name, Reserved) of
        true ->
            eTodoDB:moveTodosToTaskList(UserName, ?defTaskList, [Name]),
            Last = case UserCfg#userCfg.lastTaskList of
                       Name -> ?defTaskList;
                       Other -> Other
                   end,
            eTodoDB:saveUserCfg(UserCfg#userCfg{lists = Current -- [Name],
                                                lastTaskList = Last}),
            {ok, nil};
        false ->
            {error, invalid_list}
    end.

add_list(UserName, Name) ->
    Reserved = [?defTaskList, ?defInbox, ?defLoggedWork],
    Valid = Name =/= [] andalso length(Name) =< 100 andalso
        not eTodoUtils:containsIllegalChars(Name) andalso
        lists:all(fun(C) -> C >= 32 andalso C =/= 127 end, Name) andalso
        not lists:member(Name, Reserved) andalso
        %% Integer names would clash with sub-task parents.
        eTodoUtils:tryInt(Name) =:= Name,
    case is_owner(UserName) andalso Valid of
        true ->
            #userCfg{lists = Configured} = eTodoDB:readUserCfg(UserName),
            Current = case Configured of
                          undefined -> [];
                          Lists -> Lists
                      end,
            case lists:member(Name, Current) of
                true ->
                    {ok, unicode:characters_to_binary(Name)};
                false ->
                    UserCfg = eTodoDB:readUserCfg(UserName),
                    eTodoDB:saveUserCfg(UserCfg#userCfg{lists = Current ++ [Name]}),
                    {ok, unicode:characters_to_binary(Name)}
            end;
        false ->
            {error, invalid_list}
    end.

set_reminder(Token, Csrf, Uid, Enabled, Date, Time, Recurrence, EndDate,
             Email) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            save_reminder(unicode:characters_to_list(UserName), Uid, Enabled,
                          Date, Time, Recurrence, EndDate, Email);
        {error, _} = Error ->
            Error
    end.

update_task(Token, Csrf, Uid, Status, Priority, Description, Comment,
            Progress, DueTime, Estimate, Remaining, Lists) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            update_task(unicode:characters_to_list(UserName), Uid, Status,
                        Priority, Description, Comment, Progress, DueTime,
                        Estimate, Remaining, Lists);
        {error, _} = Error ->
            Error
    end.

create_task(Token, Csrf, List, Status, Priority, Description, Comment,
            Progress, DueTime, Estimate, Remaining) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            create_task(unicode:characters_to_list(UserName), to_list(List),
                        Status, Priority, Description, Comment, Progress,
                        DueTime, Estimate, Remaining);
        {error, _} = Error ->
            Error
    end.

delete_task(Token, Csrf, Uid) ->
    case gen_server:call(?MODULE, {authorize_write, Token, Csrf}) of
        {ok, UserName} ->
            delete_task(unicode:characters_to_list(UserName), Uid);
        {error, _} = Error ->
            Error
    end.

init([]) ->
    eTodoOidc:init(),
    {ok, #{}}.

handle_call({login, UserName0, Password0}, _From, Sessions) ->
    UserName = to_list(UserName0),
    Password = to_binary(Password0),
    case UserName =/= [] andalso Password =/= <<>> andalso
         valid_credentials(UserName, Password) of
        true ->
            Now = erlang:system_time(second),
            Sessions1 = remove_expired_sessions(Sessions, Now),
            Token = binary:encode_hex(crypto:strong_rand_bytes(32)),
            Csrf = binary:encode_hex(crypto:strong_rand_bytes(32)),
            Session = {unicode:characters_to_binary(UserName),
                       Now + ?SESSION_LIFETIME, Csrf},
            {reply, {ok, Token}, maps:put(Token, Session, Sessions1)};
        false ->
            {reply, {error, nil}, Sessions}
    end;
handle_call({external_login, UserName0}, _From, Sessions) ->
    UserName = to_list(UserName0),
    case known_user(UserName) of
        true ->
            Now = erlang:system_time(second),
            Sessions1 = remove_expired_sessions(Sessions, Now),
            Token = binary:encode_hex(crypto:strong_rand_bytes(32)),
            Csrf = binary:encode_hex(crypto:strong_rand_bytes(32)),
            Session = {unicode:characters_to_binary(UserName),
                       Now + ?SESSION_LIFETIME, Csrf},
            {reply, {ok, Token}, maps:put(Token, Session, Sessions1)};
        false ->
            {reply, {error, nil}, Sessions}
    end;
handle_call({session_user, Token}, _From, Sessions) ->
    Now = erlang:system_time(second),
    case maps:find(Token, Sessions) of
        {ok, {UserName, Expires, _Csrf}} when Expires > Now ->
            {reply, {ok, UserName}, Sessions};
        {ok, _Expired} ->
            {reply, {error, nil}, maps:remove(Token, Sessions)};
        error ->
            {reply, {error, nil}, Sessions}
    end;
handle_call({logout, Token}, _From, Sessions) ->
    {reply, ok, maps:remove(Token, Sessions)};
handle_call({todo_data, Token, ListName, ShowAll}, _From, Sessions) ->
    Now = erlang:system_time(second),
    case maps:find(Token, Sessions) of
        {ok, {UserName, Expires, Csrf}} when Expires > Now ->
            case make_todo_data(unicode:characters_to_list(UserName),
                                to_list(ListName), Csrf, ShowAll) of
                {ok, Data} ->
                    {reply, {ok, Data}, Sessions};
                error ->
                    {reply, {error, <<"invalid_list">>}, Sessions}
            end;
        _ ->
            {reply, {error, <<"invalid_session">>}, Sessions}
    end;
handle_call({settings_data, Token}, _From, Sessions) ->
    Now = erlang:system_time(second),
    case maps:find(Token, Sessions) of
        {ok, {UserName, Expires, Csrf}} when Expires > Now ->
            {reply, {ok, settings_json(unicode:characters_to_list(UserName), Csrf)},
             Sessions};
        _ ->
            {reply, {error, <<"invalid_session">>}, Sessions}
    end;
handle_call({status_form, Token}, _From, Sessions) ->
    Now = erlang:system_time(second),
    case maps:find(Token, Sessions) of
        {ok, {UserName, Expires, Csrf}} when Expires > Now ->
            {reply, {ok, {Csrf, is_owner(unicode:characters_to_list(UserName))}},
             Sessions};
        _ ->
            {reply, {error, <<"invalid_session">>}, Sessions}
    end;
handle_call({authorize_write, Token, Csrf}, _From, Sessions) ->
    Now = erlang:system_time(second),
    case maps:find(Token, Sessions) of
        {ok, {UserName, Expires, SessionCsrf}} when Expires > Now ->
            case secure_equal_binary(to_binary(Csrf), SessionCsrf) of
                true ->
                    {reply, {ok, UserName}, Sessions};
                false ->
                    {reply, {error, invalid_csrf}, Sessions}
            end;
        _ ->
            {reply, {error, invalid_session}, Sessions}
    end;
handle_call(_Request, _From, Sessions) ->
    {reply, {error, nil}, Sessions}.

handle_cast(_Request, Sessions) ->
    {noreply, Sessions}.

handle_info(_Info, Sessions) ->
    {noreply, Sessions}.

terminate(_Reason, _Sessions) ->
    ok.

code_change(_OldVersion, Sessions, _Extra) ->
    {ok, Sessions}.

known_user(UserName) ->
    case application:get_env(eTodo, user) of
        {ok, Owner0} ->
            Owner = to_list(Owner0),
            UserName =:= Owner orelse
                lists:member(UserName, guest_users(Owner));
        undefined ->
            false
    end.

valid_credentials(UserName, Password) ->
    case application:get_env(eTodo, user) of
        {ok, Owner0} ->
            Owner = to_list(Owner0),
            case UserName =:= Owner of
                true ->
                    #userCfg{webPassword = WebPassword} =
                        eTodoDB:readUserCfg(Owner),
                    secure_equal(Password, WebPassword);
                false ->
                    case lists:member(UserName, guest_users(Owner)) of
                        true ->
                            secure_equal(Password, ePeerCircle:getPwd());
                        false ->
                            false
                    end
            end;
        undefined ->
            false
    end.

secure_equal(Password, StoredPassword) when is_binary(StoredPassword) ->
    secure_equal_binary(Password, StoredPassword);
secure_equal(Password, StoredPassword) when is_list(StoredPassword) ->
    secure_equal_binary(Password, unicode:characters_to_binary(StoredPassword));
secure_equal(_Password, _StoredPassword) ->
    false.

secure_equal_binary(<<>>, _StoredPassword) ->
    false;
secure_equal_binary(_Password, <<>>) ->
    false;
secure_equal_binary(Password, StoredPassword) ->
    PasswordHash = crypto:hash(sha256, Password),
    StoredHash = crypto:hash(sha256, StoredPassword),
    secure_equal_hashes(PasswordHash, StoredHash, 0) =:= 0.

secure_equal_hashes(<<>>, <<>>, Difference) ->
    Difference;
secure_equal_hashes(<<Left, LeftRest/binary>>,
                    <<Right, RightRest/binary>>, Difference) ->
    secure_equal_hashes(LeftRest, RightRest,
                        Difference bor (Left bxor Right)).

make_todo_data(UserName, ListName, Csrf, ShowAll) ->
    #userCfg{lists = ConfiguredLists} = eTodoDB:readUserCfg(UserName),
    UserLists = case ConfiguredLists of
                    undefined -> [];
                    Lists -> Lists
                end,
    AvailableLists = [?defTaskList |
                      [Name || Name <- UserLists, Name =/= ?defTaskList]],
    case lists:member(ListName, AvailableLists) of
        false ->
            error;
        true ->
            Settings = web_settings(UserName),
            Flt = case ShowAll of
                      true -> [];
                      false -> filter(UserName, ListName, Settings)
                  end,
            Todos = sort_todos(Settings,
                               eTodoDB:getETodos(UserName, ListName, Flt,
                                                 "", undefined)),
            AllCount = length(eTodoDB:getETodos(UserName, ListName, [], "",
                                                undefined)),
            Reminders = maps:from_list(
                          [{R#alarmCfg.uid, R} ||
                              R <- eTodoDB:getReminders(UserName)]),
            JsonData = #{
                <<"lists">> => [to_binary(Name) || Name <- AvailableLists],
                <<"selected_list">> => to_binary(ListName),
                <<"csrf">> => Csrf,
                <<"can_edit">> => is_owner(UserName),
                <<"show_all">> => ShowAll,
                <<"filtered_out">> => max(0, AllCount - length(Todos)),
                <<"todos">> => [todo_json(Todo, Reminders) || Todo <- Todos]
            },
            {ok, iolist_to_binary(json:encode(JsonData))}
    end.

guest_users(User) ->
    %% Minimize guests to those users which the peer has shared todos with.
    #userCfg{ownerCfg = OwnerCfg} = eTodoDB:readUserCfg(User),
    Modify = fun (ExternalUser) ->
                     case eTodoUtils:getPeerInfo(ExternalUser) of
                         {ExtUser, _} ->
                             ExtUser;
                         _ ->
                             ExternalUser
                     end
             end,
    Owner2 = lists:map(Modify, default_list(OwnerCfg)),
    Users  = (eTodoDB:getUsers() ++ Owner2) -- [User],
    Filter = fun(WUser) ->
                     case eTodoDB:getTodosSharedWith(User, WUser) of
                         [] ->
                             false;
                         _ ->
                             true
                     end
             end,
    lists:filter(Filter, Users).

default_list(undefined) -> [];
default_list(List) -> List.

web_settings(UserName) ->
    case eTodoDB:readUserCfg(UserName) of
        #userCfg{webSettings = Settings} when is_list(Settings) -> Settings;
        _ -> []
    end.

setting(Key, Settings) ->
    case proplists:get_value(Key, Settings, ?descDef) of
        Value when is_list(Value) -> Value;
        _ -> ?descDef
    end.

status_options() ->
    [?descDef, ?descPlanning, ?descInProgress, ?descDone, ?descNA, ?descAll].

priority_options() ->
    [?descDef, ?descLow, ?descMedium, ?descHigh, ?descNA, ?descAll].

sort_options() ->
    [?descDef, ?status, ?prio, ?description, ?comment, ?createTime, ?dueTime,
     ?doneTimestamp].

settings_json(UserName, Csrf) ->
    Settings = web_settings(UserName),
    Field = fun(Key, Options) ->
                    #{<<"value">> => to_binary(setting(Key, Settings)),
                      <<"options">> => [to_binary(Option) || Option <- Options]}
            end,
    iolist_to_binary(json:encode(#{
        <<"csrf">> => Csrf,
        <<"filter_status">> => Field("filterStatus", status_options()),
        <<"filter_prio">> => Field("filterPrio", priority_options()),
        <<"sort_order">> => Field("sortOrder", sort_options()),
        <<"sort_order_sec">> => Field("sortOrderSec", sort_options())
    })).

save_settings(UserName, Values) ->
    Allowed = #{"filterStatus" => status_options(),
                "filterPrio" => priority_options(),
                "sortOrder" => sort_options(),
                "sortOrderSec" => sort_options()},
    case lists:all(fun({Key, Value}) ->
                           lists:member(Value, maps:get(Key, Allowed))
                   end, Values) of
        true ->
            UserCfg = eTodoDB:readUserCfg(UserName),
            Old = case UserCfg#userCfg.webSettings of
                      Current when is_list(Current) -> Current;
                      _ -> []
                  end,
            New = lists:foldl(fun({Key, Value}, Acc) ->
                                      lists:keystore(Key, 1, Acc, {Key, Value})
                              end, Old, Values),
            eTodoDB:saveUserCfg(UserCfg#userCfg{webSettings = New}),
            {ok, nil};
        false ->
            {error, invalid_settings}
    end.

%% "Default" uses the filter saved in the user's configuration (the one set
%% in the desktop client); a chosen status or priority overrides that part.
filter(UserName, ListName, Settings) ->
    Saved = case eTodoDB:readUserCfg(UserName) of
                #userCfg{filter = Filters} when is_list(Filters) -> Filters;
                _ -> []
            end,
    Default = case lists:keyfind(ListName, 1, Saved) of
                  {ListName, ListFilter} when is_list(ListFilter) -> ListFilter;
                  _ -> [Flag || Flag <- Saved, not is_tuple(Flag)]
              end,
    Filter1 = apply_filter(setting("filterStatus", Settings), fun status_flag/1,
                           [?statusPlanning, ?statusInProgress, ?statusDone,
                            ?statusNone], Default),
    apply_filter(setting("filterPrio", Settings), fun prio_flag/1,
                 [?prioLow, ?prioMedium, ?prioHigh, ?prioNone], Filter1).

apply_filter(?descDef, _ToFlag, _All, Filter) ->
    Filter;
apply_filter(?descAll, _ToFlag, All, Filter) ->
    Filter -- All;
apply_filter(Value, ToFlag, All, Filter) ->
    case ToFlag(Value) of
        undefined -> Filter;
        Flag -> (Filter -- All) ++ lists:delete(Flag, All)
    end.

status_flag(?descPlanning) -> ?statusPlanning;
status_flag(?descInProgress) -> ?statusInProgress;
status_flag(?descDone) -> ?statusDone;
status_flag(?descNA) -> ?statusNone;
status_flag(_) -> undefined.

prio_flag(?descLow) -> ?prioLow;
prio_flag(?descMedium) -> ?prioMedium;
prio_flag(?descHigh) -> ?prioHigh;
prio_flag(?descNA) -> ?prioNone;
prio_flag(_) -> undefined.

%% The sort is stable, so sorting by the secondary key first keeps it
%% as the tie-breaker for the primary key.
sort_todos(Settings, Todos) ->
    Sorted = sort_by(setting("sortOrderSec", Settings), Todos),
    sort_by(setting("sortOrder", Settings), Sorted).

sort_by(?descDef, Todos) -> Todos;
sort_by(?status, Todos) -> sort_key(fun(T) -> status_rank(T#etodo.status) end, Todos);
sort_by(?prio, Todos) -> sort_key(fun(T) -> prio_rank(T#etodo.priority) end, Todos);
sort_by(?description, Todos) ->
    sort_key(fun(T) -> string:to_lower(T#etodo.description) end, Todos);
sort_by(?comment, Todos) ->
    sort_key(fun(T) -> string:to_lower(T#etodo.comment) end, Todos);
sort_by(?createTime, Todos) -> sort_key(fun(T) -> T#etodo.createTime end, Todos);
sort_by(?dueTime, Todos) -> sort_key(fun(T) -> T#etodo.dueTime end, Todos);
sort_by(?doneTimestamp, Todos) -> sort_key(fun(T) -> T#etodo.doneTime end, Todos);
sort_by(_, Todos) -> Todos.

sort_key(Fun, Todos) ->
    [Todo || {_, Todo} <- lists:keysort(1, [{Fun(Todo), Todo} || Todo <- Todos])].

status_rank(?descInProgress) -> 1;
status_rank(?descPlanning) -> 2;
status_rank(?descDone) -> 4;
status_rank(_) -> 3.

prio_rank(?descHigh) -> 1;
prio_rank(?descMedium) -> 2;
prio_rank(?descLow) -> 3;
prio_rank(_) -> 4.

todo_json(ETodo = #etodo{uidDB = UidDB}, Reminders) ->
    Json = todo_json(ETodo),
    Json#{<<"reminder">> => reminder_json(maps:get(UidDB, Reminders,
                                                   undefined))}.

todo_json(#etodo{uid = Uid,
                 description = Description,
                 status = Status,
                 statusDB = StatusDB,
                 priority = Priority,
                 dueTimeDB = DueTimeDB,
                 hasSubTodo = HasSubTodo,
                 comment = Comment,
                 owner = Owner,
                 sharedWith = SharedWith,
                 createTimeDB = CreateTimeDB,
                 doneTimeDB = DoneTimeDB,
                 progress = Progress,
                 lists = Lists,
                 listsDB = ListsDB}) ->
    {Estimate, Remaining} = eTodoDB:getTime(Uid),
    #{
        <<"uid">> => to_binary(Uid),
        <<"description">> => to_binary(Description),
        <<"status">> => to_binary(Status),
        <<"priority">> => to_binary(Priority),
        <<"due_time">> => due_time(DueTimeDB),
        <<"has_sub_todos">> => HasSubTodo,
        <<"done">> => StatusDB =:= done,
        <<"comment">> => to_binary(Comment),
        <<"owner">> => to_binary(Owner),
        <<"shared_with">> => to_binary(SharedWith),
        <<"created">> => due_time(CreateTimeDB),
        <<"done_time">> => due_time(DoneTimeDB),
        <<"progress">> => progress(Progress),
        <<"lists">> => to_binary(Lists),
        <<"lists_db">> => [to_binary(List) || List <- ListsDB],
        <<"estimate">> => to_binary(Estimate),
        <<"remaining">> => to_binary(Remaining)
    }.

progress(Progress) when is_integer(Progress) ->
    integer_to_binary(Progress);
progress(_Progress) ->
    <<>>.

is_owner(UserName) ->
    case application:get_env(eTodo, user) of
        {ok, Owner} -> UserName =:= to_list(Owner);
        undefined -> false
    end.

parse_uid(UidBin) when is_binary(UidBin) ->
    try {ok, binary_to_integer(UidBin)}
    catch error:badarg -> error
    end;
parse_uid(_) ->
    error.

update_done(UserName, UidBin, Done) ->
    case is_owner(UserName) andalso parse_uid(UidBin) of
        {ok, Uid} ->
            Visible = [E || E = #etodo{uidDB = U} <-
                                eTodoDB:getETodos(UserName, ?defTaskList,
                                                  [], "", undefined),
                            U =:= Uid],
            case {Visible, eTodoDB:getTodo(Uid)} of
                {[_ | _], Todo = #todo{}} ->
                    Status = case Done of
                                 true -> done;
                                 false -> planning
                             end,
                    DoneTime = eTodoUtils:doneTime(Todo#todo.doneTime, Status),
                    Todo2 = Todo#todo{status = Status, doneTime = DoneTime},
                    eTodoDB:updateTodo(UserName, Todo2),
                    try eTodo:todoUpdated(UserName, Todo2)
                    catch _:_ -> ok
                    end,
                    {ok, due_time(DoneTime)};
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, forbidden}
    end.

update_task(UserName, UidBin, Status0, Priority0, Description0, Comment0,
            Progress0, DueTime0, Estimate0, Remaining0, Lists0) ->
    case {is_owner(UserName) andalso parse_uid(UidBin),
          task_values(Status0, Priority0, Description0, Comment0, Progress0,
                      DueTime0, Estimate0, Remaining0)} of
        {{ok, Uid}, {ok, Values}} ->
            Visible = [E || E = #etodo{uidDB = U} <-
                                eTodoDB:getETodos(UserName, ?defTaskList,
                                                  [], "", undefined),
                            U =:= Uid],
            case {Visible, eTodoDB:getTodo(Uid)} of
                {[_ | _], Todo = #todo{}} ->
                    #{status := Status, priority := Priority,
                      description := Description, comment := Comment,
                      progress := Progress, due_time := DueTime,
                      estimate := Estimate, remaining := Remaining} = Values,
                    DoneTime = eTodoUtils:doneTime(Todo#todo.doneTime, Status),
                    Todo2 = Todo#todo{status = Status, priority = Priority,
                                      description = Description,
                                      comment = Comment, progress = Progress,
                                      dueTime = DueTime, doneTime = DoneTime},
                    eTodoDB:updateTodo(UserName, Todo2),
                    eTodoDB:saveTime(Uid, Estimate, Remaining),
                    assign_lists(UserName, Uid, Lists0),
                    try eTodo:todoUpdated(UserName, Todo2)
                    catch _:_ -> ok
                    end,
                    {ok, nil};
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, invalid_task}
    end.

%%--------------------------------------------------------------------
%% Reminders, the same settings as the reminder dialog of the desktop
%% client. The command to run is not offered on the web; an existing one
%% is kept.
%%--------------------------------------------------------------------
-define(recurrences, [none, time15, time30, time1h, time2h, time4h, timeDay,
                      timeWeek, timeMonth, timeYear]).

reminder_json(#alarmCfg{startDate = {_, _, _} = Date, startTime = Time,
                        recurrence = Recurrence, endDate = EndDate,
                        emailReminder = Email, nextAlarm = Next}) ->
    #{<<"enabled">> => true,
      <<"date">> => iso_date(Date),
      <<"time">> => iso_time(Time),
      <<"recurrence">> => atom_to_binary(recurrence_or_none(Recurrence)),
      <<"end_date">> => iso_date(EndDate),
      <<"email">> => Email =:= true,
      <<"next">> => case Next of
                        {{_, _, _} = D, T} ->
                            <<(iso_date(D))/binary, " ",
                              (iso_time(T))/binary>>;
                        _ ->
                            <<>>
                    end};
reminder_json(_) ->
    #{<<"enabled">> => false, <<"date">> => <<>>, <<"time">> => <<>>,
      <<"recurrence">> => <<"none">>, <<"end_date">> => <<>>,
      <<"email">> => false, <<"next">> => <<>>}.

recurrence_or_none(Recurrence) ->
    case lists:member(Recurrence, ?recurrences) of
        true -> Recurrence;
        false -> none
    end.

iso_date({Y, M, D}) when is_integer(Y) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]));
iso_date(_) ->
    <<>>.

iso_time({H, M}) -> iso_time({H, M, 0});
iso_time({H, M, _S}) when is_integer(H) ->
    iolist_to_binary(io_lib:format("~2..0B:~2..0B", [H, M]));
iso_time(_) ->
    <<>>.

save_reminder(UserName, UidBin, Enabled, Date0, Time0, Recurrence0, EndDate0,
              Email) ->
    case {is_owner(UserName) andalso parse_uid(UidBin),
          reminder_values(Enabled, Date0, Time0, Recurrence0, EndDate0)} of
        {{ok, Uid}, {ok, {Date, Time, Recurrence, EndDate}}} ->
            Old = eTodoDB:getReminder(UserName, Uid),
            New = case Old of
                      #alarmCfg{} -> Old;
                      _ -> #alarmCfg{uid = Uid, userName = UserName}
                  end,
            New2 = New#alarmCfg{startDate = Date, startTime = Time,
                                recurrence = Recurrence, endDate = EndDate,
                                emailReminder = Email =:= true andalso
                                                    Date =/= undefined,
                                nextAlarm = undefined, lastAlarm = undefined},
            Unchanged = is_record(Old, alarmCfg) andalso
                Old#alarmCfg{nextAlarm = undefined, lastAlarm = undefined}
                =:= New2,
            case {Old, Date} of
                _ when Unchanged ->
                    %% Keep when it fires next.
                    {ok, nil};
                {#alarmCfg{execCmd = undefined}, undefined} ->
                    eTodoDB:delReminder(Old),
                    {ok, nil};
                {undefined, undefined} ->
                    {ok, nil};
                _ ->
                    eTodoDB:addReminder(New2),
                    {ok, nil}
            end;
        _ ->
            {error, invalid_reminder}
    end.

reminder_values(false, _Date, _Time, _Recurrence, _EndDate) ->
    {ok, {undefined, undefined, none, undefined}};
reminder_values(true, Date0, Time0, Recurrence0, EndDate0) ->
    case {date(Date0), time_of_day(Time0), recurrence(Recurrence0),
          date(EndDate0)} of
        {{ok, Date = {_, _, _}}, {ok, Time}, {ok, Recurrence}, {ok, EndDate}} ->
            {ok, {Date, Time, Recurrence, EndDate}};
        _ ->
            error
    end.

time_of_day(Value) ->
    case string:tokens(string:trim(to_list(Value)), ":") of
        [] ->
            {ok, undefined};
        [H, M | _] ->
            try {list_to_integer(H), list_to_integer(M)} of
                {Hour, Min} when Hour >= 0, Hour < 24, Min >= 0, Min < 60 ->
                    {ok, {Hour, Min}};
                _ ->
                    error
            catch error:badarg -> error
            end;
        _ ->
            error
    end.

recurrence(Value) ->
    case [R || R <- ?recurrences,
               atom_to_list(R) =:= string:trim(to_list(Value))] of
        [Recurrence] -> {ok, Recurrence};
        [] -> error
    end.

%% A task is always in the "All tasks" list. An empty selection keeps the
%% current lists, and lists which don't exist are ignored.
assign_lists(_UserName, _Uid, []) ->
    ok;
assign_lists(UserName, Uid, Lists0) ->
    #userCfg{lists = ConfiguredLists} = eTodoDB:readUserCfg(UserName),
    Available = [?defTaskList |
                 case ConfiguredLists of
                     undefined -> [];
                     Configured -> Configured
                 end],
    Requested = [to_list(List) || List <- Lists0],
    Chosen = lists:usort([?defTaskList |
                          [List || List <- Requested,
                                   lists:member(List, Available)]]),
    Current = eTodoDB:getLists(UserName, Uid),
    %% Sub-tasks are attached to their parent task, which is not a list.
    Extra = [List || List <- Current, not lists:member(List, Available)],
    case lists:usort(Current) =:= lists:usort(Chosen ++ Extra) of
        true ->
            ok;
        false ->
            Parents = [eTodoUtils:tryInt(List) || List <- Extra],
            eTodoDB:assignLists(UserName, Uid, Chosen ++ Parents)
    end.

create_task(UserName, List, Status0, Priority0, Description0, Comment0,
            Progress0, DueTime0, Estimate0, Remaining0) ->
    #userCfg{lists = ConfiguredLists} = eTodoDB:readUserCfg(UserName),
    Available = [?defTaskList |
                 case ConfiguredLists of
                     undefined -> [];
                     Configured -> Configured
                 end],
    Description = string:trim(to_list(Description0)),
    case {is_owner(UserName) andalso lists:member(List, Available),
          Description =/= [],
          task_values(Status0, Priority0, Description, Comment0, Progress0,
                      DueTime0, Estimate0, Remaining0)} of
        {true, true, {ok, Values}} ->
            #{status := Status, priority := Priority, description := Desc,
              comment := Comment, progress := Progress, due_time := DueTime,
              estimate := Estimate, remaining := Remaining} = Values,
            Uid = eTodoUtils:makeRef(),
            Todo = #todo{uid = Uid, status = Status, priority = Priority,
                         description = Desc, comment = Comment,
                         progress = Progress, dueTime = DueTime,
                         doneTime = eTodoUtils:doneTime(undefined, Status),
                         createTime = {date(), time()}, owner = UserName,
                         sharedWith = []},
            Row = eTodoDB:getRow(UserName, List),
            eTodoDB:addTodo(#userInfo{userName = UserName, uid = Uid, row = Row,
                                      parent = eTodoUtils:tryInt(List)}, Todo),
            eTodoDB:saveTime(Uid, Estimate, Remaining),
            try eTodo:todoCreated(List, Row, Todo)
            catch _:_ -> ok
            end,
            {ok, to_binary(Uid)};
        {false, _, _} ->
            {error, forbidden};
        _ ->
            {error, invalid_task}
    end.

delete_task(UserName, UidBin) ->
    case is_owner(UserName) andalso parse_uid(UidBin) of
        {ok, Uid} ->
            Visible = [E || E = #etodo{uidDB = U} <-
                                eTodoDB:getETodos(UserName, ?defTaskList,
                                                  [], "", undefined),
                            U =:= Uid],
            case Visible of
                [_ | _] ->
                    eTodoDB:delTodo(Uid, UserName),
                    {ok, nil};
                [] ->
                    {error, not_found}
            end;
        _ ->
            {error, forbidden}
    end.

task_values(Status0, Priority0, Description0, Comment0, Progress0,
            DueTime0, Estimate0, Remaining0) ->
    case {status(to_list(Status0)), priority(to_list(Priority0)),
          progress_percent(Progress0), date(DueTime0),
          nonnegative_integer(Estimate0), nonnegative_integer(Remaining0)} of
        {{ok, Status}, {ok, Priority}, {ok, Progress}, {ok, DueTime},
         {ok, Estimate}, {ok, Remaining}} ->
            {ok, #{status => Status, priority => Priority,
                   description => to_list(Description0),
                   comment => to_list(Comment0), progress => Progress,
                   due_time => DueTime, estimate => Estimate,
                   remaining => Remaining}};
        _ ->
            error
    end.

status("Planning") -> {ok, planning};
status("In progress") -> {ok, inProgress};
status("Done") -> {ok, done};
status("N/A") -> {ok, undefined};
status(_) -> error.

priority("Low") -> {ok, low};
priority("Medium") -> {ok, medium};
priority("High") -> {ok, high};
priority("N/A") -> {ok, undefined};
priority(_) -> error.

progress_percent(Value) ->
    case nonnegative_integer(Value) of
        {ok, Number} when Number =< 100 -> {ok, Number};
        _ -> error
    end.

nonnegative_integer(Value) ->
    try list_to_integer(string:trim(to_list(Value))) of
        Number when Number >= 0 -> {ok, Number};
        _ -> error
    catch error:badarg -> error
    end.

date(Value) ->
    case string:trim(to_list(Value)) of
        [] -> {ok, undefined};
        Date ->
            case string:tokens(Date, "-") of
                [Year, Month, Day] ->
                    try {list_to_integer(Year), list_to_integer(Month),
                         list_to_integer(Day)} of
                        Result ->
                            case calendar:valid_date(Result) of
                                true -> {ok, Result};
                                false -> error
                            end
                    catch error:badarg -> error
                    end;
                _ ->
                    error
            end
    end.

%% The GUI date picker can store an out-of-range placeholder date
%% when no due date is set.
due_time({Year, _Month, _Day} = Date)
  when is_integer(Year), Year >= 1900, Year =< 9999 ->
    to_binary(eTodoUtils:toStr(Date));
due_time({{Year, _Month, _Day}, _Time} = Date)
  when is_integer(Year), Year >= 1900, Year =< 9999 ->
    to_binary(eTodoUtils:toStr(Date));
due_time(_) ->
    <<>>.

remove_expired_sessions(Sessions, Now) ->
    maps:filter(fun(_Token, {_UserName, Expires, _Csrf}) -> Expires > Now end,
                Sessions).

to_list(Value) when is_binary(Value) ->
    unicode:characters_to_list(Value);
to_list(Value) when is_list(Value) ->
    Value;
to_list(_Value) ->
    [].

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(Value);
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 2}, compact]);
to_binary(_Value) ->
    <<>>.
