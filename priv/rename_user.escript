#!/usr/bin/env escript
%%% Renames an eTodo user in the Mnesia database.
%%%
%%% Usage (with the eTodo node stopped, as the user that owns the database):
%%%   ERL_AFLAGS="-sname eTodo" escript rename_user.escript MnesiaDir From To [--apply]
%%%
%%% Without --apply nothing is written, only counts are printed.
%%%
%%% Tasks that are shared with other users (or owned by someone else) are left
%%% untouched, including their userInfo records, so the new user does not see
%%% them. All other tables are renamed completely.

-define(PLAIN_TABLES, [userCfg, listCfg, logWork, alarmCfg, messages, conCfg,
                       httpd_user, httpd_group]).

main([Dir, From, To | Rest]) ->
    Apply = Rest =:= ["--apply"],
    application:set_env(mnesia, dir, Dir),
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables(mnesia:system_info(tables), 60000),
    {atomic, Result} = mnesia:transaction(fun() -> migrate(From, To, Apply) end),
    [io:format("~-12s ~p~n", [atom_to_list(T), N]) || {T, N} <- Result],
    case Apply of
        true  -> io:format("Applied.~n");
        false -> io:format("Dry run, nothing written. Use --apply to write.~n")
    end,
    mnesia:stop(),
    halt(0);
main(_) ->
    io:format("Usage: rename_user.escript MnesiaDir From To [--apply]~n"),
    halt(1).

migrate(From, To, Apply) ->
    Shared = sharedUids(From),
    Todos = migrateTodos(From, To, Shared, Apply),
    Infos = migrateUserInfo(From, To, Shared, Apply),
    Plain = [{T, migrateTable(T, From, To, Apply)} || T <- ?PLAIN_TABLES],
    [{todo, Todos}, {userInfo, Infos}, {skipped_shared, length(Shared)} | Plain].

%% A task is shared if someone else owns it or is in sharedWith.
sharedUids(From) ->
    [element(2, T) || T <- mnesia:select(todo, [{'_', [], ['$_']}]),
                      isShared(From, element(11, T), element(12, T))].

isShared(From, SharedWith, Owner) ->
    OtherOwner = Owner =/= undefined andalso Owner =/= From,
    Others = case SharedWith of
                 L when is_list(L) -> [U || U <- L, U =/= From];
                 _ -> []
             end,
    OtherOwner orelse Others =/= [].

migrateTodos(From, To, Shared, Apply) ->
    Changed = [begin
                   New = setelement(11, setelement(12, T, rename(From, To, element(12, T))),
                                    rename(From, To, element(11, T))),
                   Apply andalso mnesia:write(New),
                   New
               end || T <- mnesia:select(todo, [{'_', [], ['$_']}]),
                      not lists:member(element(2, T), Shared),
                      rename(From, To, element(11, T)) =/= element(11, T)
                          orelse rename(From, To, element(12, T)) =/= element(12, T)],
    length(Changed).

migrateUserInfo(From, To, Shared, Apply) ->
    Old = [R || {userInfo, U, Uid, _, _} = R
                    <- mnesia:select(userInfo, [{'_', [], ['$_']}]),
                U =:= From, not lists:member(Uid, Shared)],
    [begin
         Apply andalso mnesia:delete_object(R),
         Apply andalso mnesia:write(setelement(2, R, To))
     end || R <- Old],
    length(Old).

migrateTable(Table, From, To, Apply) ->
    Type = mnesia:table_info(Table, type),
    Old = [R || R <- mnesia:select(Table, [{'_', [], ['$_']}]),
                rename(From, To, R) =/= R],
    [begin
         New = rename(From, To, R),
         Apply andalso begin
                           delete(Table, Type, R),
                           mnesia:write(New)
                       end
     end || R <- Old],
    length(Old).

delete(Table, bag, R)  -> mnesia:delete_object(Table, R, write);
delete(Table, _, R)    -> mnesia:delete({Table, element(2, R)}).

%% Replaces the exact string From wherever it occurs as a whole value.
rename(From, To, From) -> To;
rename(From, To, T) when is_tuple(T) ->
    list_to_tuple([rename(From, To, E) || E <- tuple_to_list(T)]);
rename(From, To, L) when is_list(L) -> [rename(From, To, E) || E <- L];
rename(_, _, Other) -> Other.
