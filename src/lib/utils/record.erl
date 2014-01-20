-module(record).

-export([to_json/1, attributes/1, attributes/2, map_attributes/1, map_attributes/2]).

%%% RecordValue can't be atom
-spec(to_json(record()) -> JSONString::binary()).
to_json(Record) ->
    json:encode(attributes(Record)).

-spec(attributes(record()) -> [{}]|[{Key::any(), Value::any()}, ...]).
attributes(Record) when Record =:= undefined ->
    [{}];
attributes(Record) ->
    % io:format("Record: ~p~n", [Record]),
    [Name|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Name),
    attributes(Fields, Values, []).

-spec(attributes(Record::record(), ExceptKeys::[atom()]) ->
      [{}]|[{Key::any(), Value::any()}, ...]).
attributes(Record, _ExceptKeys) when Record =:= undefined ->
    [{}];
attributes(Record, ExceptKeys) ->
    [Name|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Name),
    Proplist = attributes(Fields, Values, []),
    if
        ExceptKeys =:= [] ->
            Proplist;
        true ->
            {_, Attributes} = proplists:split(Proplist, ExceptKeys),
            Attributes
    end.


-spec(map_attributes([record()]) -> []|[[{Key::any(), Value::any()}, ...]]).
map_attributes(Records) when Records =:= [] orelse Records =:= undefined ->
    [];
map_attributes(Records) ->
    map_attributes(Records, [], []).

map_attributes(Records, _ExceptKeys) when Records =:= [] orelse Records =:= undefined ->
    [];
map_attributes(Records, ExceptKeys) ->
    map_attributes(Records, ExceptKeys, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

attributes([], [], Result) ->
    Result;
attributes([Field|TailFields], [Value|TailValues], Result) when Value =:= undefined ->
    attributes(TailFields, TailValues, [{Field, null}|Result]);
attributes([Field|TailFields], [{<<ObjectId:12/binary>>}|TailValues], Result) ->
    BinaryId = db:objectid_to_binary_string({ObjectId}),
    attributes(TailFields, TailValues, [{Field, BinaryId}|Result]);
attributes([Field|TailFields], [{MegaSecs, Secs, _MicroSecs}|TailValues], Result) ->
    UnixTime = MegaSecs * 1000000 + Secs,
    attributes(TailFields, TailValues, [{Field, UnixTime}|Result]);
attributes([Field|TailFields], [Value|TailValues], Result) ->
    attributes(TailFields, TailValues, [{Field, Value}|Result]).

map_attributes([], _ExceptKeys, Result) ->
    Result;
map_attributes([Record|TailRecords], ExceptKeys, Result) when Record =:= undefined ->
    map_attributes(TailRecords, ExceptKeys, Result);
map_attributes([Record|TailRecords], ExceptKeys, Result) ->
    map_attributes(TailRecords, ExceptKeys, [attributes(Record, ExceptKeys)|Result]).
