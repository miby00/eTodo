%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @author Anders Ramsell <anders.ramsell.1727@student.uu.se>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%% Functions used by eTodo*
%%% @end
%%% Created : 13 Jul 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(eTodoUtils).

-include("eTodo.hrl").

-include_lib("wx/include/wx.hrl").

%% API
-export([makeStr/1, toStr/1, toStr/2, toDB/1, toDB/2,
         taskInternal/1, taskExternal/1, convertUid/1, convertUid/2,
         col/2, default/2, doneTime/2, dateTime/0, makeETodo/3,
         makeRef/0, getRootDir/0, apply/4, toColumn/1, toStatusDB/1,
         addDateTime/2, tryInt/1, cancelTimer/1, getIp/0, getWeekDay/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
makeStr(undefined) -> "";
makeStr(List)  -> makeStr(lists:reverse(List), "").

makeStr([], SoFar) -> SoFar;
makeStr([Value | Rest], "")    -> makeStr(Rest, toStr(Value));
makeStr([Value | Rest], SoFar) -> makeStr(Rest, toStr(Value) ++ ";" ++ SoFar).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
doneTime(undefined, done)                        -> dateTime();
doneTime(_DoneTime, Status) when Status =/= done -> undefined;
doneTime(DoneTime, _)                            -> DoneTime.

%%--------------------------------------------------------------------
%% @doc  Return current local time.
%% @spec dateTime() -> DateTime
%% @end
%%--------------------------------------------------------------------
dateTime() ->
    {date(), time()}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
toStr({Hour, Min, Sec}, time) ->
    addZero(toStr(Hour))  ++ ":" ++
        addZero(toStr(Min))   ++ ":" ++
        addZero(toStr(Sec)).

toStr(inProgress) -> ?descInProgress;
toStr(planning)   -> ?descPlanning;
toStr(done)       -> ?descDone;
toStr(low)        -> ?descLow;
toStr(medium)     -> ?descMedium;
toStr(high)       -> ?descHigh;
toStr(undefined)  -> ?descNone;

toStr(Value) when is_integer(Value) ->
    integer_to_list(Value);
toStr(Value) when is_atom(Value) ->
    atom_to_list(Value);
toStr(Value) when is_binary(Value) ->
    binary_to_list(Value);
toStr(Value) when is_list(Value) ->
    Value;
toStr({Year, Month, Day}) ->
    toStr(Year) ++ "-" ++
        addZero(toStr(Month)) ++ "-" ++
        addZero(toStr(Day));
toStr({{Year, Month, Day}, {Hour, Min}}) ->
    toStr(Year) ++ "-" ++
        addZero(toStr(Month)) ++ "-" ++
        addZero(toStr(Day))   ++ " " ++
        addZero(toStr(Hour))  ++ ":" ++
        addZero(toStr(Min));
toStr({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    toStr(Year) ++ "-" ++
        addZero(toStr(Month)) ++ "-" ++
        addZero(toStr(Day))   ++ " " ++
        addZero(toStr(Hour))  ++ ":" ++
        addZero(toStr(Min))   ++ ":" ++
        addZero(toStr(Sec)).

addZero(Num) when length(Num) == 1 ->
    "0" ++ Num;
addZero(Num) ->
    Num.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
tryInt(Value) ->
    case catch list_to_integer(Value) of
        {'EXIT', _} ->
            Value;
        IntValue ->
            IntValue
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
toDB(?descInProgress) -> inProgress;
toDB(?descPlanning)   -> planning;
toDB(?descDone)       -> done;
toDB(?descNA)         -> undefined;
toDB(?descLow)        -> low;
toDB(?descMedium)     -> medium;
toDB(?descHigh)       -> high.

toDB(undefined, time) ->
    undefined;
toDB("", time) ->
    undefined;
toDB(Time, time) ->
    case string:tokens(Time, ":-") of
        [Year, Month, Days] ->
            case catch toIntTuple(Year, Month, Days) of
                {'EXIT', _Reason} ->
                    Time;
                IntTuple ->
                    IntTuple
            end;
        _ ->
            Time
    end.

toIntTuple(Year, Month, Days) ->
    {list_to_integer(Year),
     list_to_integer(Month),
     list_to_integer(Days)}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
taskInternal(?uid)           -> uid;
taskInternal(?status)        -> status;
taskInternal(?prio)          -> priority;
taskInternal(?dueTime)       -> dueTime;
taskInternal(?description)   -> description;
taskInternal(?comment)       -> comment;
taskInternal(?sharedWith)    -> sharedWith;
taskInternal(?createTime)    -> createTime;
taskInternal(?doneTimestamp) -> doneTime;
taskInternal(?owner)         -> owner.

taskExternal(uid)           -> ?uid;
taskExternal(status)        -> ?status;
taskExternal(priority)      -> ?prio;
taskExternal(dueTime)       -> ?dueTime;
taskExternal(description)   -> ?description;
taskExternal(comment)       -> ?comment;
taskExternal(sharedWith)    -> ?sharedWith;
taskExternal(createTime)    -> ?createTime;
taskExternal(doneTime)      -> ?doneTimestamp;
taskExternal(owner)         -> ?owner.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
col(Desc, Columns) ->
    case catch lists:keyfind(Desc, 2, Columns) of
        {Col, _Desc} when Col =/= 'EXIT' ->
            Col;
        _ ->
            0
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
default(undefined, Default)  -> Default;
default("",        Default)  -> Default;
default(Value    , _Default) -> Value.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
apply(Module, Function, Args, Default) ->
    try apply(Module, Function, Args)  of
        Value ->
            default(Value, Default)
    catch
        _:_ ->
            Default
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
makeRef() ->
    erlang:phash2({erlang:unique_integer(),
                   erlang:timestamp(),
                   net_adm:localhost()}).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
getRootDir() ->
    code:priv_dir(eTodo).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
makeETodo(#todo{uid         = Uid,
                priority    = Priority,
                status      = Status,
                doneTime    = DoneTime,
                dueTime     = DueTime,
                createTime  = CreateTime,
                comment     = Comment,
                description = Desc,
                progress    = Progress,
                sharedWith  = SharedWith,
                owner       = Owner}, User, Columns) ->

    ShareText = makeStr(default(SharedWith, [User])),
    Lists     = eTodoDB:getLists(User, Uid),

    #etodo{status         = toStr(Status),
           statusCol      = col(?status, Columns),
           statusDB       = Status,
           priority       = toStr(Priority),
           priorityCol    = col(?prio, Columns),
           priorityDB     = Priority,
           owner          = toStr(default(Owner, User)),
           ownerCol       = col(?owner, Columns),
           dueTime        = toStr(DueTime),
           dueTimeCol     = col(?dueTime, Columns),
           dueTimeDB      = DueTime,
           description    = toStr(Desc),
           descriptionCol = col(?description, Columns),
           comment        = toStr(Comment),
           commentCol     = col(?comment, Columns),
           sharedWith     = ShareText,
           sharedWithCol  = col(?sharedWith, Columns),
           sharedWithDB   = default(SharedWith, [User]),
           createTime     = toStr(CreateTime),
           createTimeCol  = col(?createTime, Columns),
           createTimeDB   = CreateTime,
           doneTime       = toStr(DoneTime),
           doneTimeCol    = col(?doneTimestamp, Columns),
           doneTimeDB     = DoneTime,
           hasSubTodo     = eTodoDB:hasSubTodo(Uid),
           uid            = toStr(Uid),
           uidCol         = col(?uid, Columns),
           uidDB          = Uid,
           progress       = Progress,
           lists          = makeStr(Lists),
           listsDB        = Lists}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
toColumn(?statusPlanning)   -> ?status;
toColumn(?statusInProgress) -> ?status;
toColumn(?statusDone)       -> ?status;
toColumn(?statusNone)       -> ?status;
toColumn(?prioLow)          -> ?prio;
toColumn(?prioMedium)       -> ?prio;
toColumn(?prioHigh)         -> ?prio;
toColumn(?prioNone)         -> ?prio;
toColumn(_)                 -> ?uid.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
toStatusDB(?statusPlanning)   -> planning;
toStatusDB(?statusInProgress) -> inProgress;
toStatusDB(?statusDone)       -> done;
toStatusDB(?statusNone)       -> undefined.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
getIp() ->
    {ok, Addrs} = inet:getif(),
    IPAddr = getIp(Addrs),
    case IPAddr of
        {{N1, N2, N3, N4}, _, _} ->
            toStr(N1) ++ "." ++ toStr(N2) ++ "." ++
                toStr(N3) ++ "." ++ toStr(N4);
        _ ->
            "127.0.0.0"
    end.

getIp(Addrs) ->
    lists:keyfind({255, 255, 255, 0}, 3, Addrs).

%%======================================================================
%% Function : convertUid(Uid) -> NewUid
%% Purpose  : Convert uid to a string and back again.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    : Uid is an erlang ref.
%%======================================================================
convertUid(Uid) when is_list(Uid) ->
    binary_to_term(base64:decode(list_to_binary(Uid)));
convertUid(Uid) ->
    binary_to_list(base64:encode(term_to_binary({uid, Uid}))).

convertUid(Uid, Date) ->
    binary_to_list(base64:encode(term_to_binary({uidAndDate, Uid, Date}))).

%%======================================================================
%% Function : addDateTime({Date, Time}, {AddDate, AddTime}) -> NewDateTime
%% Purpose  : Calculate a new date time after adding time.
%% Types    :
%%----------------------------------------------------------------------
%% Notes    :
%%======================================================================
addDateTime({Date, Time}, {AddDate, AddTime}) ->
    Date2 = addDate(Date, AddDate),
    addTime({Date2, Time}, AddTime).

addTime(DateTime, Time) ->
    S = calendar:datetime_to_gregorian_seconds(DateTime) +
        calendar:time_to_seconds(Time),
    calendar:gregorian_seconds_to_datetime(S).

addDate(Date, {Y, M, D}) ->
    Date2 = addYears(Date, Y),
    Date3 = addMonths(Date2, M),
    addDays(Date3, D).

addYears({Y, M, D}, Years) ->
    {Y + Years, M, D}.

addMonths({Y, M, D}, 0) ->
    {Y, M, D};
addMonths({Y, M, D}, Months) ->
    checkMonth({Y, M + Months, D}).


checkMonth({Y, M, D}) when (M > 12) ->
    checkMonth({Y+1, M-12, D});
checkMonth({Y, M, D}) ->
    Last = calendar:last_day_of_the_month(Y, M),
    {Y, M, min(D, Last)}.

addDays({Y, M, D}, Days) ->
    checkDate({Y, M, (D + Days)}).

checkDate({Y, M, D}) when (M > 12) ->
    checkDate({Y+1, M-12, D});
checkDate({Y, M, D}) ->
    case calendar:last_day_of_the_month(Y, M) of
        Last when (Last < D) ->
            checkDate({Y, M+1, (D - Last)});
        _ ->
            {Y, M, D}
    end.

cancelTimer(undefined) ->
    ok;
cancelTimer(Ref) ->
    %% Recieved pong, cancel timer.
    erlang:cancel_timer(Ref).

getWeekDay(Date) ->
    DayNum = calendar:day_of_the_week(Date),
    lists:nth(DayNum, ["Monday", "Tuesday", "Wednesday",
                       "Thursday", "Friday", "Saturday", "Sunday"]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
