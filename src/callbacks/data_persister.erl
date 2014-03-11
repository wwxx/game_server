%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

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
    io:format("ModelName: ~p, Id: ~p~n", [ModelName, Id]),
    io:format("SelectorRecord: ~p~n", [SelectorRecord]),
    case player_data:ets_find(SelectorRecord) of
        undefined ->
            ok;
        Rec ->
            ModifierRecord = record_mapper:only_fields(Rec, ChangedFields),
            db:update_by(uuid, Id, ModifierRecord)
    end;
data_persist(ModelName, Id, Status, _Value) when Status =:= create ->
    SelectorRecord = make_selector(ModelName, Id),
    case player_data:ets_find(SelectorRecord) of
        undefined -> ok;
        Rec -> db:create(Rec)
    end.

make_selector(ModelName, Id) ->
    EmptyFieldValues = record_mapper:get_empty_field_values(ModelName),
    Record = list_to_tuple([ModelName|EmptyFieldValues]),
    record_mapper:set_field(Record, 'uuid', Id).
