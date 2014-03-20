-module(model).
-export([update/2,
         find/1,
         all/1,
         where/1,
         delete/1,
         create/1,
         create/2,
         count/1,
         persist_table/1,
         persist_all/0]).

-include("include/common_const.hrl").

find(Selector) ->
    [Table|Values] = tuple_to_list(Selector),
    ensure_load_data(Table),
    case hd(Values) of
        undefined -> selectOne(Table, Values);
        Id -> get({Table, Id})
    end.

all(Table) ->
    ensure_load_data(Table),
    case id_status_list(Table) of
        [] -> [];
        IdList ->
            lists:foldl(fun
                ({Id, _}, Result) ->
                    case get({Table, Id}) of
                        undefined -> undefined;
                        Rec -> [Rec|Result]
                    end
            end, [], IdList)
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
            put({Table, Id}, update_record(Rec, Modifier))
    end.

match_update(Table, Selector, Modifier) ->
    case id_status_list(Table) of
        [] -> ok;
        IdList ->
            Fields = record_mapper:get_mapping(Table),
            [_|Values] = tuple_to_list(Selector),
            FieldsAndValues = makepat(Fields, Values),
            lists:foreach(fun
                ({Id, _}) ->
                    Rec = get({Table, Id}),
                    case match(Rec, FieldsAndValues) of
                        true  ->
                            update_status(Table, Id, ?MODEL_UPDATE),
                            put({Table, Id}, update_record(Rec, Modifier));
                        false -> undefined
                    end
            end, IdList)
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
            end, [], IdList),
            ok
    end.

%% create new record.
create(Record) ->
    RecWithId = case tuple_to_list(Record) of
        [RTable, undefined|RValues] ->
            NewId = uuid_factory:gen(),
            list_to_tuple([RTable,NewId|RValues]);
        _ -> Record
    end,
    [Table, Id|_Values] = tuple_to_list(RecWithId),
    update_status(Table, Id, ?MODEL_CREATE),
    put({Table, Id}, RecWithId).

%% Load data from databse.
create(Record, load) ->
    [Table, Id|_] = tuple_to_list(Record),
    update_status(Table, Id, ?MODEL_ORIGIN),
    put({Table, Id}, Record).

persist_table(Table) ->
    case id_status_list(Table) of
        [] -> ok;
        IdList ->
            case delete_status_list(Table) of
                [] -> [];
                DeleteIdList ->
                    case sqls(Table, DeleteIdList ++ IdList) of
                        [] -> ok;
                        Sqls -> db:execute(binary_string:join(Sqls, <<";">>))
                    end
            end
    end.

persist_all() ->
    lists:foreach(fun persist_table/1, all_loaded_tables()).

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
        true  ->
            error_logger:info_msg("Load data from Cache!~n"),
            true;
        false ->
            error_logger:info_msg("Load data from DB!~n"),
            PlayerID = get(player_id),
            Module = list_to_atom(atom_to_list(Table) ++ "_model"),
            case Module:load_data(PlayerID) of
                {ok, []} ->
                    error_logger:info_msg("Module: ~p, PlayerID: ~p, no data found!~n", [Module, PlayerID]),
                    undefined;
                {ok, Recs} ->
                    error_logger:info_msg("ensure_load_data: ~p~n", [Recs]),
                    lists:foreach(fun
                        (Rec) -> create(Rec, load)
                    end, Recs)
            end,
            record_loaded_table(Table)
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
            put({loaded, tables}, [Table|Tables])
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
                Status =:= ?MODEL_ORIGIN -> Result;
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
    {Table, Fields, Values}.

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
