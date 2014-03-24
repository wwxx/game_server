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
%%%        This module s used to map RecordName to Record Structures.
%%%        And provide some functions to modify record value and get record name, etc.
%%% @end
%%% Created :  四 10 10 14:49:39 2013 by Savin-Max µ

-module(record_mapper).

-behaviour(gen_server).

%% API
-export([start_link/1,
		 add_mapping/1,
		 add_mappings/1,
		 get_mapping/1,
         get_empty_field_values/1,
		 is_mapped/1,
		 has_field/2,
		 get_type/1,
		 get_field/2,
		 set_field/3,
         get_fields/2,
         set_fields/2,
         except_fields/2,
         only_fields/2,
         unmap/2
        ]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).


%% We store the ETS table ID across calls.
-record(state, {ets_table_id}).

%% External functions

%% @doc Spawns a registered process on the local node that stores the structure of records in an ETS table.
%%
%% @spec start_link(integer()) -> {ok, pid()}
%% @end
start_link(EtsTableId) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [EtsTableId], []).

%% @doc Specfies the field identifiers associated with a record name.
-spec(add_mapping({atom(), FieldIds::list(atom())}) -> ok).
add_mapping({RecordName, FieldIds} = RecordDescriptor) ->
	assert_is_mapping(RecordDescriptor),
	server_call(add_mapping, {RecordName, FieldIds}).

%% @doc Specfies the field identifiers associated with a list of record names.
-spec(add_mappings(list({RecordName::atom(), FieldIds::list(atom())})) -> ok).
add_mappings(RecordDescriptorList) ->
	[assert_is_mapping(Mapping) || Mapping <- RecordDescriptorList],
	[add_mapping(Mapping) || Mapping <- RecordDescriptorList],
	ok.

%% @doc Gets the field identifiers associated with a record name.
-spec(get_mapping(atom()) -> list(atom())).
get_mapping(RecordName) when is_atom(RecordName) ->
	[{RecordName, FieldIds}] = ets:lookup(?TAB, RecordName),
	FieldIds.

get_empty_field_values(RecordName) when is_atom(RecordName) ->
    FieldIds = get_mapping(RecordName),
    lists:map(fun(_) -> undefined end, FieldIds).


%% @doc Returns whether a record is mapped. The argument can be either an atom (a possible record name)
%%      or a tuple (a possible record).
-spec(is_mapped(RecordOrRecordName::atom()|tuple()) -> boolean()).
is_mapped(RecordName) when is_atom(RecordName) ->
	case get_mapping(RecordName) of
		[] ->
			false;
		[{RecordName, _FieldIds}] ->
			true
	end;
is_mapped(Record) when is_tuple(Record), size(Record) > 0 ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	case get_mapping(RecordName) of
		[] ->
			false;
		[{RecordName, FieldIds}] ->
			length(FieldIds) =:= length(FieldValues)
	end;
is_mapped(_) ->
	false.

%% @doc Returns whether a record has an '_id' field. The argument can be either an atom (a possible record name)
%%      or a tuple (a possible record).
-spec(has_field(RecordOrRecordName::atom()|tuple(), Field::atom()) -> boolean()).
has_field(RecordName, Field) when is_atom(RecordName) andalso is_atom(Field) ->
	FieldIds = get_mapping(RecordName),
	CheckHasId = fun(FieldId, Result) ->
									   Result or (FieldId =:= Field)
				 end,
	lists:foldl(CheckHasId, false, FieldIds);
has_field(Record, Field) when is_tuple(Record), size(Record) > 0 andalso is_atom(Field) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	has_field(RecordName, Field) andalso length(FieldValues) =:= length(get_mapping(RecordName)).

%% @doc Gets the value of a field from a record.
-spec(get_field(record(), atom()) -> any()).
get_field(Record, Field) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	get_field(FieldIds, FieldValues, Field).

get_fields(Record, Fields) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
    get_fields(FieldIds, FieldValues, Fields, []).

%% @doc set the expceted fields to undefined.
-spec(except_fields(record(), [atom()]) -> record()).
except_fields(Record, Fields) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
    NewFieldValues = except_fields(FieldIds, FieldValues, Fields, []),
    list_to_tuple([RecordName|NewFieldValues]).

%% @doc only keep fields' value and set others to undefined.
-spec(only_fields(record(), [atom()]) -> record()).
only_fields(Record, Fields) ->
	[RecordName|FieldValues] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
    NewFieldValues = only_fields(FieldIds, FieldValues, Fields, []),
    list_to_tuple([RecordName|NewFieldValues]).

%% @doc Sets the value of a field in a record. If the value is a reference to a document,
%%      a callback function is invoked to map the referenced document to a record. An updated record
%%      is returned.
-spec(set_field(record(), atom(), FieldValue::any()) -> record()).
set_field(Record, FieldId, FieldValue) ->
	[RecordName|RecordList] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	UpdatedRecordList = set_field(RecordList, FieldIds, FieldId, FieldValue, []),
	list_to_tuple([RecordName|UpdatedRecordList]).

-spec(set_fields(Record::record(),
                 ElementSpec::[{FieldId::atom(), Value::any()}]) -> record()).
set_fields(Record, ElementSpec) ->
	[RecordName|Values] = tuple_to_list(Record),
	FieldIds = get_mapping(RecordName),
	UpdatedRecordList = set_fields(Values, FieldIds, ElementSpec, []),
	list_to_tuple([RecordName|UpdatedRecordList]).

%% @doc A convenience function that extracts the first element of a tuple. If the
%%      tuple is a record, the first element contains the record name.
-spec(get_type(record()) -> atom()).
get_type(Record) ->
	true = is_mapped(Record),
	[RecordName|_] = tuple_to_list(Record),
	RecordName.

unmap(Collection, Document) ->
	CallbackFunc = fun(Coll, Id) ->
						   {Reference} = mongo:find_one(Coll, {'_id', Id}),
						   Reference
				   end,
	mongrel_mapper:unmap(Collection, Document, CallbackFunc).

%% Server functions

%% @private
%% @doc Initializes the server with the ETS table used to store the
%%      mappings needed for mapping records to documents.
-spec(init(EtsTableId::list(integer())) -> {ok, #state{}}).
init([EtsTableId]) ->
	{ok, #state{ets_table_id = EtsTableId}}.

%% @private
%% @doc Responds synchronously to server calls. This function is invoked when a mapping is
%%      added or a mapping needs to be retrieved.
-spec(handle_call(Message::tuple(), From::pid(), State::#state{}) -> {reply, Reply::any(), NewState::record()}).
handle_call({add_mapping, {Key, Value}}, _From, State) ->
	true = ets:insert(State#state.ets_table_id, {Key, Value}),
	{reply, ok, State}.

%% @private
%% @doc Responds asynchronously to messages. Asynchronous messages are ignored.
-spec(handle_cast(any(), State::#state{}) -> {no_reply, State::#state{}}).
handle_cast(_Message, State) ->
	{noreply, State}.

%% @private
%% @doc Responds to non-OTP messages. Non-OTP messages are ignored.
-spec(handle_info(any(), State::#state{}) -> {no_reply, State::#state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
%% @doc Handles the shutdown of the server.
-spec(terminate(any(), #state{}) -> ok).
terminate(_Reason, _State) ->
	ok.

%% @private
%% @doc Responds to code changes.
-spec(code_change(any(), State::#state{}, any()) -> {ok, State::#state{}}).
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% Internal functions
server_call(Command, Args) ->
	gen_server:call(?SERVER, {Command, Args}, infinity).

get_field([FieldId|_IdTail], [FieldValue|_ValuesTail], FieldId) ->
	FieldValue;
get_field([_FieldIdHead|IdTail], [_FieldValueHead|ValuesTail], FieldId) ->
	get_field(IdTail, ValuesTail, FieldId).

set_field([], [], _FieldId, _NewFieldValue, Result) ->
	Result;
set_field([_FieldValue|ValuesTail], [FieldId|_TailsList], FieldId, NewFieldValue, Result) ->
	Result ++ [NewFieldValue] ++ ValuesTail;
set_field([FieldValue|ValuesTail], [_FieldId|TailsList], FieldId, NewFieldValue, Result) ->
	set_field(ValuesTail, TailsList, FieldId, NewFieldValue, Result ++ [FieldValue]).

only_fields([], [], [], Result) ->
    lists:reverse(Result);
only_fields([_FieldId|TailFieldIds], [_FieldValue|TailFieldValues], [], Result) ->
    only_fields(TailFieldIds, TailFieldValues, [], [undefined|Result]);
only_fields([FieldId|TailFieldIds], [FieldValue|TailFieldValues], Fields, Result) ->
    case lists:delete(FieldId, Fields) of
        Fields ->
            only_fields(TailFieldIds, TailFieldValues, Fields, [undefined|Result]);
        NewFields ->
            only_fields(TailFieldIds, TailFieldValues, NewFields, [FieldValue|Result])
    end.

except_fields([], [], [], Result) ->
    lists:reverse(Result);
except_fields([_FieldId|TailFieldIds], [FieldValue|TailFieldValues], [], Result) ->
    except_fields(TailFieldIds, TailFieldValues, [], [FieldValue|Result]);
except_fields([FieldId|TailFieldIds], [FieldValue|TailFieldValues], Fields, Result) ->
    case lists:delete(FieldId, Fields) of
        Fields ->
            except_fields(TailFieldIds, TailFieldValues, Fields, [FieldValue|Result]);
        NewFields ->
            except_fields(TailFieldIds, TailFieldValues, NewFields, [undefined|Result])
    end.

get_fields(_TailFieldIds, _TailFieldValues, [], Result) ->
    lists:reverse(Result);
get_fields([FieldId|TailFieldIds], [FieldValue|TailFieldValues], Fields, Result) ->
    error_logger:info_msg("FieldId: ~p, TailFieldIds:~p, Fields:~p~n", 
                          [FieldId, TailFieldIds, Fields]),
    case lists:delete(FieldId, Fields) of
        Fields ->
            get_fields(TailFieldIds, TailFieldValues, Fields, Result);
        NewFields ->
            get_fields(TailFieldIds, TailFieldValues, NewFields, [FieldValue|Result])
    end.

set_fields([], [], _ElementSpec, Result) ->
    lists:reverse(Result);
set_fields([Value|TailValues], [_Field|TailFieldIds], [], Result) ->
    set_fields(TailValues, TailFieldIds, [], [Value|Result]);
set_fields([Value|TailValues], [FieldId|TailFieldIds], ElementSpec, Result) ->
    [NewResult, NewElementSpec] = case proplists:get_value(FieldId, ElementSpec) of
        undefined ->
            [[Value|Result], ElementSpec];
        NewValue ->
            [[NewValue|Result], proplists:delete(FieldId, ElementSpec)]
    end,
    set_fields(TailValues, TailFieldIds, NewElementSpec, NewResult).

assert_is_mapping({RecordName, FieldIds}) when is_atom(RecordName) ->
	[true = is_atom(FieldId) || FieldId <- FieldIds].
