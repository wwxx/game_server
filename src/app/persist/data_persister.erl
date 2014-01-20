%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
%%% @doc
%%%        Callbacks invoked by player process for persisting player data.
%%% @end
%%% Created :  一 10 14 01:13:27 2013 by Savin-Max µ

-module(data_persister).

-behaviour(data_persist_behavior).

-export([persist/4]).

%%%===================================================================
%%% Framework Callbacks
%%%===================================================================

-spec(persist(ModelName::atom(),
          Id::any(),
          Status::delete|update|create,
          Value::undefined|[ChangedField::atom(), ...]) -> ok).
persist(ModelName, Id, Status, Value) ->
    % Invoke your own data persist function.
    foundation_war_data_persist(ModelName, Id, Status, Value).


%%%===================================================================
%%% Custom data persist functions
%%%===================================================================

foundation_war_data_persist(ModelName, Id, Status, _Value) when Status =:= delete ->
    case db:get_father_info(ModelName) of
        {Coll, _FatherKey} ->
            Key = list_to_atom(atom_to_list(ModelName) ++ "._id"),
            db:mongo_modify(Coll, {Key, Id}, {'$pull', {ModelName, {'_id', Id}}});
        undefined ->
            SelectorRecord = make_selector(ModelName, Id),
            db:delete_one(SelectorRecord)
    end;
foundation_war_data_persist(ModelName, Id, Status, ChangedFields) when Status =:= update ->
    SelectorRecord = make_selector(ModelName, Id),
    case player_data:find(SelectorRecord) of
        undefined ->
            ok;
        Rec ->
            case db:get_father_info(ModelName) of
                {Coll, _FatherKey} ->
                    Key = list_to_atom(atom_to_list(ModelName) ++ "._id"),
                    MongoDoc = generate_mongo_doc(Rec, ChangedFields),
                    db:mongo_modify(Coll, {Key, Id}, {'$set', {ModelName, MongoDoc}});
                undefined ->
                    ModifierRecord = record_mapper:only_fields(Rec, ChangedFields),
                    db:update(SelectorRecord, ModifierRecord)
            end
    end;
foundation_war_data_persist(ModelName, Id, Status, _Value) when Status =:= create ->
    SelectorRecord = make_selector(ModelName, Id),
    case player_data:find(SelectorRecord) of
        undefined ->
            ok;
        Rec ->
            case db:get_father_info(ModelName) of
                {Coll, FatherKey} ->
                    MongoDoc = unmap_record(Rec),
                    FatherDocId = record_mapper:get_field(Rec, FatherKey),
                    db:mongo_modify(Coll,
                                    {'_id', FatherDocId},
                                    {'$push', {ModelName, MongoDoc}});
                undefined ->
                    db:create(Rec)
            end
    end.

make_selector(ModelName, Id) ->
    EmptyFieldValues = record_mapper:get_empty_field_values(ModelName),
    Record = list_to_tuple([ModelName|EmptyFieldValues]),
    record_mapper:set_field(Record, '_id', Id).

unmap_record(Record) ->
    [Name|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Name),
    unmap_record(Values, Fields, []).

unmap_record([], [], Result) ->
    list_to_tuple(lists:reverse(Result));
unmap_record([Value|Values], [Field|Fields], Result) ->
    unmap_record(Values, Fields, [Value, Field|Result]).

generate_mongo_doc(Record, Fields) ->
    generate_mongo_doc(Fields, Record, []).

generate_mongo_doc([], Record, Result) ->
    list_to_tuple(Result);
generate_mongo_doc([Field|TailFields], Record, Result) ->
    Value = record_mapper:get_field(Record, Field),
    generate_mongo_doc(TailFields, Record, [Field, Value|Result]).
