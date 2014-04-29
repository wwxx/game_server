-module(model).
-export([update/2,
         update/1,
         find/1,
         find_or_create/1,
         all/1,
         all/2,
         where/1,
         delete/1,
         create/1,
         create/2,
         count/1,
         %persist_table/1,
         persist_all/0]).

-include("include/common_const.hrl").

find(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case hd(Values) of
        undefined -> selectOne(Table, Values);
        Id -> get({Table, Id})
    end.

find_or_create(Selector) ->
    case find(Selector) of
        undefined -> create(Selector);
        Rec -> Rec
    end.

all(Table) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec -> [Rec|Result]
                    end
            end, [], IdList)
    end.

all(Table, {Offset, Limit}) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            SubIdList = lists:sublist(IdList, Offset, Limit),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec -> [Rec|Result]
                    end
            end, [], SubIdList)
    end.

where(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            FieldsAndValues = makepat(Fields, Values),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  -> [Rec|Result];
                        false -> Result
                    end
            end, [], IdList)
    end.

count(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    Fields = record_mapper:get_mapping(Table),
    case makepat(Fields, Values) of
        [] -> length(id_status_list(Table));
        FieldsAndValues ->
            case id_status_list(Table) of
                [] -> 0;
                IdList ->
                    lists:foldl(fun
                        ({Id, _}, Result) ->
                            Rec = get({Table, Id}),
                            case match(Rec, FieldsAndValues) of
                                true  -> Result + 1;
                                false -> Result
                            end
                    end, 0, IdList)
            end
    end.

update(Record) ->
    [Table, Id|_] = tuple_to_list(Record),
    case get({Table, Id}) of
        undefined -> ok;
        _ ->
            update_status(Table, Id, ?MODEL_UPDATE),
            put({Table, Id}, Record),
            Record
    end.

update(Selector, Modifier) ->
    [Table|SelectorValues] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case hd(SelectorValues) of
        undefined -> match_update(Table, Selector, Modifier);
        Id -> update_by_key(Table, Id, Modifier)
    end.

update_by_key(Table, Id, Modifier) ->
    case get({Table, Id}) of
        undefined -> ok;
        Rec ->
            update_status(Table, Id, ?MODEL_UPDATE),
            NewRec = update_record(Rec, Modifier),
            put({Table, Id}, NewRec),
            NewRec
    end.

match_update(Table, Selector, Modifier) ->
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            [_|Values] = tuple_to_list(Selector),
            FieldsAndValues = makepat(Fields, Values),
            lists:foldl(fun
                ({Id, _}, Result) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_UPDATE),
                            NewRec = update_record(Rec, Modifier),
                            put({Table, Id}, NewRec),
                            [NewRec|Result];
                        false -> 
                            Result
                    end
            end, [], IdList)
    end.

delete(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case hd(Values) of
        undefined ->
            Fields = record_mapper:get_mapping(Table),
            FieldsAndValues = makepat(Fields, Values),
            match_delete(Table, FieldsAndValues);
        Id ->
            update_status(Table, Id, ?MODEL_DELETE),
            erase({Table, Id})
    end.

match_delete(Table, FieldsAndValues) ->
    case id_status_list(Table) of
        [] -> ok;
        IdList ->
            lists:foreach(fun
                ({Id, _}) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_DELETE),
                            erase({Table, Id});
                        false -> undefined
                    end
            end, IdList),
            ok
    end.

%% create new record.
create(Records) when is_list(Records) ->
    lists:foreach(fun create/1, Records);
create(Record) ->
    RecWithId = case tuple_to_list(Record) of
        [RTable, undefined|RValues] ->
            NewId = uuid_factory:gen(),
            list_to_tuple([RTable,NewId|RValues]);
        _ -> Record
    end,
    [Table, Id|_Values] = tuple_to_list(RecWithId),
    ensure_load_data(Table),
    update_status(Table, Id, ?MODEL_CREATE),
    put({Table, Id}, RecWithId),
    RecWithId.

%% Load data from databse.
create(Record, load) ->
    [Table, Id|_] = tuple_to_list(Record),
    update_status(Table, Id, ?MODEL_ORIGIN),
    put({Table, Id}, Record).

%persist_table(Table) ->
    %error_logger:info_msg("Table: ~p, will be persist!~n", [Table]),
    %case generate_persist_sql(Table) of
        %<<>> -> do_nothing;
        %Sql ->
            %execute_with_procedure(Sql),
            %reset_status(Table)
    %end.

generate_persist_sql(Table) ->
    case id_status_list(Table) of
        [] -> <<>>;
        IdList ->
            DeleteIdList = delete_status_list(Table),
            case sqls(Table, DeleteIdList ++ IdList) of
                [] -> <<>>;
                Sqls ->
                    Sql = binary_string:join(Sqls, <<";">>),
                    Sql
            end
    end.

persist_all() ->
    try do_persist_all() of
        Result -> Result
    catch
        Type:Msg ->
            exception:notify(Type, Msg)
    end.

do_persist_all() ->
    Tables = all_loaded_tables(),
    % error_logger:info_msg("Tables: ~p~n", [Tables]),
    Sqls = lists:foldl(fun(Table, Result) ->
                           case generate_persist_sql(Table) of
                               <<>> -> Result;
                               Sql -> [Sql|Result]
                           end
                       end, [], Tables),
    % error_logger:info_msg("Sqls: ~p~n", [Sqls]),
    case binary_string:join(Sqls, <<";">>) of
        <<>> -> do_nothing;
        JoinedSql ->
            execute_with_procedure(JoinedSql),
            reset_tables_status(Tables)
    end.

reset_tables_status(Tables) ->
    lists:foreach(fun reset_status/1, Tables).

reset_status(Table) ->
    put({Table, deleteIdList}, []),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            NewIdList = lists:foldl(fun
                            ({Id, _}, Result) ->
                                [{Id, ?MODEL_ORIGIN}|Result]
                        end, [], IdList),
            put({Table, idList}, NewIdList)
    end.

execute_with_procedure(Sql) ->
    ProcedureName = db:procedure_name(<<"player">>, get(player_id)),
    db:execute_with_procedure(ProcedureName, Sql).

%% Private Methods
selectOne(Table, Values) ->
    Fields = record_mapper:get_mapping(Table),
    case id_status_list(Table) of
        [] -> undefined;
        IdList ->
            FieldsAndValues = makepat(Fields, Values),
            selectOne(Table, IdList, FieldsAndValues)
    end.

selectOne(_, [], _) ->
    undefined;
selectOne(Table, [{Id, _}|IdList], FieldsAndValues) ->
    Rec = get({Table, Id}),
    case match(Rec, FieldsAndValues) of
        true -> Rec;
        false -> selectOne(Table, IdList, FieldsAndValues)
    end.

update_status(Table, Id, Status) ->
    case id_status_list(Table) of
        [] ->
            put({Table, idList}, [{Id, Status}]);
        IdList ->
            case lists:keyfind(Id, 1, IdList) of
                false ->
                    put({Table, idList}, [{Id, Status}|IdList]);
                {Id, ?MODEL_CREATE} when Status =:= ?MODEL_UPDATE ->
                    do_nothing;
                {Id, ?MODEL_CREATE} when Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, ?MODEL_CREATE}, IdList),
                    put({Table, idList}, NewIdList);
                {Id, OldStatus} when Status =:= ?MODEL_DELETE ->
                    NewIdList = lists:delete({Id, OldStatus}, IdList),
                    put({Table, idList}, NewIdList),
                    add_to_delete_list(Table, Id);
                {Id, OldStatus} ->
                    NewIdList = lists:delete({Id, OldStatus}, IdList),
                    put({Table, idList}, [{Id, Status}|NewIdList])
            end
    end.

makepat(Fields, Values) ->
    makepat(Fields, Values, []).

makepat([], [], Result) ->
    lists:reverse(Result);
makepat([_Field|Fields], [Value|Values], Result) when Value =:= undefined ->
    makepat(Fields, Values, Result);
makepat([Field|Fields], [Value|Values], Result) ->
    makepat(Fields, Values, [{Field, Value}|Result]).

match(Record, FieldsAndValues) ->
    [RecordName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(RecordName),
    match(Fields, Values, FieldsAndValues).

match(_, _, []) ->
    true;
match([], [], _) ->
    false;
match([Field|Fields], [Value|Values], [{Field, Value}|FieldsAndValues]) ->
    match(Fields, Values, FieldsAndValues);
match([_Field|Fields], [_Value|Values], FieldsAndValues) ->
    match(Fields, Values, FieldsAndValues).

ensure_load_data(Table) ->
    case is_table_loaded(Table) of
        true  -> true;
        false ->
            PlayerID = get(player_id),
            Module = list_to_atom(atom_to_list(Table) ++ "_model"),
            case Module:load_data(PlayerID) of
                {ok, []} -> undefined;
                {ok, Recs} -> insert_recs(Recs, Module)
            end,
            record_loaded_table(Table),
            case erlang:function_exported(Module, after_load_data, 1) of
                true -> Module:after_load_data(PlayerID);
                false -> ok
            end
    end.

is_table_loaded(Table) ->
    case get({loaded, Table}) of
        true -> true;
        _ -> false
    end.

record_loaded_table(Table) ->
    put({loaded, Table}, true),
    case get({loaded, tables}) of
        undefined ->
            put({loaded, tables}, [Table]);
        Tables ->
            case lists:keyfind(Table, 1, Tables) of
                true -> ok;
                false -> put({loaded, tables}, [Table|Tables])
            end
    end.

all_loaded_tables() ->
    case get({loaded, tables}) of
        undefined -> [];
        Tables when is_list(Tables) -> Tables
    end.

id_status_list(Table) ->
    case get({Table, idList}) of
        undefined -> [];
        [] -> [];
        IdList -> IdList
    end.

delete_status_list(Table) ->
    case get({Table, deleteIdList}) of
        undefined -> [];
        [] -> [];
        IdList -> IdList
    end.

sqls(Table, IdList) ->
    lists:foldl(fun
        ({Id, Status}, Result) ->
            if
                Status =:= ?MODEL_ORIGIN ->
                    Result;
                true ->
                    case get({Table, Id}) of
                        undefined -> Result;
                        Rec ->
                            [sql(Rec, Status)|Result]
                    end
            end
    end, [], IdList).

sql(Rec, ?MODEL_CREATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    sqerl:sql({insert, Table, {Fields, [Values]}});
sql(Rec, ?MODEL_UPDATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    Uuid = hd(Values),
    sqerl:sql({update, Table, map(Fields, Values), {where, {uuid, '=', Uuid}}});
sql(Rec, ?MODEL_DELETE) ->
    {Table, _Fields, Values} = rec_info(Rec),
    Uuid = hd(Values),
    sqerl:sql({delete, Table, {uuid, '=', Uuid}}).

rec_info(Rec) ->
    [Table|Values] = tuple_to_list(Rec),
    Fields = record_mapper:get_mapping(Table),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    Rule = proplists:get_value(serialize, Module:module_info(attributes)),
    SerializedValues = serialize(Values, Fields, Rule),
    {Table, Fields, SerializedValues}.

map(Fields, Values) ->
    map(Fields, Values, []).

map([], [], Result) ->
    Result;
map([_Field|Fields], [Value|Values], Result) when Value =:= undefined ->
    map(Fields, Values, Result);
map([Field|Fields], [Value|Values], Result) ->
    map(Fields, Values, [{Field, Value}|Result]).

add_to_delete_list(Table, Id) ->
    case get({Table, deleteIdList}) of
        undefined ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}]);
        [] ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}]);
        List ->
            put({Table, deleteIdList}, [{Id, ?MODEL_DELETE}|List])
    end.

update_record(Record, Modifier) ->
    [Table|RecordValues] = tuple_to_list(Record),
    [_|ModifierValues] = tuple_to_list(Modifier),
    Values = update_record(RecordValues, ModifierValues, []),
    list_to_tuple([Table|Values]).

update_record([], [], Result) ->
    lists:reverse(Result);
update_record([RecordValue|RecordValues], [ModifierValue|ModifierValues], Result) when ModifierValue =:= undefined ->
    update_record(RecordValues, ModifierValues, [RecordValue|Result]);
update_record([_RecordValue|RecordValues], [ModifierValue|ModifierValues], Result)->
    update_record(RecordValues, ModifierValues, [ModifierValue|Result]).

insert_recs(Recs, Module) ->
    Rule = proplists:get_value(serialize, Module:module_info(attributes)),
    case Rule of
        undefined -> 
            lists:foreach(fun(Rec) -> create(Rec, load) end, Recs);
        Rule ->
            Fields = record_mapper:get_mapping(hd(tuple_to_list(hd(Recs)))),
            lists:foreach(fun(Rec) -> 
                NewRec = deserialize(Rec, Fields, Rule),
                create(NewRec, load) 
            end, Recs)
    end.

deserialize(Rec, Fields, Rule) ->
    [RecName|Values] = tuple_to_list(Rec),
    TermValues = deserialize(Values, Fields, Rule, []),
    list_to_tuple([RecName|TermValues]).

deserialize([], [], _Rule, Result) -> 
    lists:reverse(Result);
deserialize([Value|Values], [Field|Fields], Rule, Result) ->
    case proplists:get_bool(Field, Rule) of
        true when Value =/= undefined -> 
            Data = base64:decode(Value),
            TermValue = binary_to_term(Data),
            deserialize(Values, Fields, Rule, [TermValue|Result]);
        _ ->
            deserialize(Values, Fields, Rule, [Value|Result])
    end.


serialize(Values, _Fields, undefined) -> Values;
serialize(Values, Fields, Rule) ->
    serialize(Values, Fields, Rule, []).

serialize([], [], _Rule, Result) ->
    lists:reverse(Result);
serialize([Value|Values], [Field|Fields], Rule, Result) ->
    case proplists:get_bool(Field, Rule) of
        true when Value =/= undefined -> 
            Data = term_to_binary(Value),
            SerializedValue = base64:encode(Data),
            serialize(Values, Fields, Rule, [SerializedValue|Result]);
        _ ->
            serialize(Values, Fields, Rule, [Value|Result])
    end.

