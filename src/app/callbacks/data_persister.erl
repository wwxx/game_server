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
    data_persist(ModelName, Id, Status, Value).


%%%===================================================================
%%% Custom data persist functions
%%%===================================================================

data_persist(ModelName, Id, Status, _Value) when Status =:= delete ->
    db:delete_by(ModelName, uuid, Id);
data_persist(ModelName, Id, Status, ChangedFields) when Status =:= update ->
    SelectorRecord = make_selector(ModelName, Id),
    case player_data:find(SelectorRecord) of
        undefined ->
            ok;
        Rec ->
            ModifierRecord = record_mapper:only_fields(Rec, ChangedFields),
            db:update_by(uuid, Id, ModifierRecord)
    end;
data_persist(ModelName, Id, Status, _Value) when Status =:= create ->
    SelectorRecord = make_selector(ModelName, Id),
    case player_data:find(SelectorRecord) of
        undefined -> ok;
        Rec -> db:create(Rec)
    end.

make_selector(ModelName, Id) ->
    EmptyFieldValues = record_mapper:get_empty_field_values(ModelName),
    Record = list_to_tuple([ModelName|EmptyFieldValues]),
    record_mapper:set_field(Record, '_id', Id).