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
%%%       Manage player's ets data:
%%% @end
%%% Created :  日 10 06 23:34:35 2013 by Savin-Max µ

-module(player_data).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_player_id/1,
         create/2,
         delete/2,
         update/3,
         find/2,
         where/2,
         count/2,
         table/1,
         clean/2,
         ets_find/1,
         get_single_record_status/3,
         get_player_record_status/2,
         get_player_records_status/1,
         all_record_status/0,
         get_data_status/1,
         set_data_status/2,
         del_record_status/3,
         get_loaded/2,
         set_loaded/3,
         flush_to_mysql/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE_TAB, player_data_state_tracker).
-define(ACTIVE_CHECK_DURATION, 600). % 6 minutes
-define(DATA_EXPIRE_DURATION, 1800). % 30 minutes

-include("include/db_schema.hrl").
-include("include/gproc_macros.hrl").

-record(state, {playerID, circulation_active_check_timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Only can be invoked by game_connection
get_player_id(Udid) ->
    users_model:get_player_id(Udid).

table(ModelName) ->
    player_data_holder:table(ModelName).

set_data_status(Key, Value) ->
    gen_server:call(?MODULE, {set_data_status, Key, Value}).

get_data_status(Key) ->
    case ets:lookup(?STATE_TAB, Key) of
        [{Key, Value}] ->
            Value;
        _ ->
            undefined
    end.

create(PlayerID, Record) ->
    track_active(PlayerID, Record),
    case validate_ownership(PlayerID, self()) of
        true ->
            NewId = uuid_factory:gen(),
            RecordWithId = record_mapper:set_field(Record, 'uuid', NewId),
            [Name|_] = tuple_to_list(Record),
            if
                Name =:= users ->
                    ets_create(NewId, RecordWithId);
                true ->
                    ets_create(PlayerID, RecordWithId)
            end;
        false ->
            io:format("Permission deny: you are not the owner of this player~n")
    end.

delete(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    ensure_data_loaded(PlayerID, SelectorRecord),
    case validate_ownership(PlayerID, self()) of
        true -> ets_delete(PlayerID, SelectorRecord);
        false -> io:format("Permission deny: you are not the owner of this player~n")
    end.

update(PlayerID, SelectorRecord, ModifierRecord) ->
    track_active(PlayerID, SelectorRecord),
    ensure_data_loaded(PlayerID, SelectorRecord),
    case validate_ownership(PlayerID, self()) of
        true -> ets_update(PlayerID, SelectorRecord, ModifierRecord);
        false -> io:format("Permission deny: you are not the owner of this player~n")
    end.

find(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    ensure_data_loaded(PlayerID, SelectorRecord),
    ets_find(SelectorRecord).

where(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    ensure_data_loaded(PlayerID, SelectorRecord),
    ets_where(SelectorRecord).

count(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    ensure_data_loaded(PlayerID, SelectorRecord),
    ets_count(SelectorRecord).

clean(PlayerID, ModelName) ->
    case validate_ownership(PlayerID, self()) of
        true ->
            set_loaded(PlayerID, ModelName, false),
            Tab = table(ModelName),
            EmptyFieldValues = record_mapper:get_empty_field_values(ModelName),
            Record = list_to_tuple([ModelName|EmptyFieldValues]),
            SelectorRecord = if
                ModelName =:= users ->
                    record_mapper:set_field(Record, uuid, PlayerID);
                true ->
                    record_mapper:set_field(Record, user_id, PlayerID)
            end,
            true = ets:match_delete(Tab, ets_utils:makepat(SelectorRecord));
        false ->
            io:format("Permission deny: you are not the owner of this player~n")
    end.

-spec(get_single_record_status(PlayerID::binary(), ModelName::atom(), Id::any()) ->
      [{Status::atom(), Value::[atom()]|undefined}]).
get_single_record_status(PlayerID, ModelName, Id) ->
    get_data_status({record_status, PlayerID, ModelName, Id}).

-spec(get_player_record_status(PlayerID::binary(), ModelName::atom()) ->
      [{Id::any(), Status::atom, Value::[atom()]|undefined}]).
get_player_record_status(PlayerID, ModelName) ->
    ets:match(?STATE_TAB, {{record_status, PlayerID, ModelName, '$1'}, {'$2', '$3'}}).
    % ets:match(?STATE_TAB, {{record_status, PlayerID, ModelName, '$1'}, $2}).

-spec(get_player_records_status(PlayerID::binary()) ->
      [{ModelName::atom(), Id::any(), Status::atom, Value::[atom()]|undefined}]).
get_player_records_status(PlayerID) ->
    ets:match(?STATE_TAB, {{record_status, PlayerID, '$1', '$2'}, {'$3', '$4'}}).

%% Flush all the cached data to mysql.
%% Need to optimize speed, currently is flushed one by one.
flush_to_mysql() ->
    lists:foreach(
        fun([PlayerID, ModelName, Id, Status, Value]) ->
            data_persister:persist(ModelName, Id, Status, Value),
            del_record_status(PlayerID, ModelName, Id)
        end, all_record_status()).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Timer = erlang:send_after(?ACTIVE_CHECK_DURATION, self(), circulation_active_check),
    {ok, #state{circulation_active_check_timer = Timer}}.

handle_call({set_data_status, Key, Value}, _From, State) ->
    Result = ets:insert(?STATE_TAB, {Key, Value}),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_active_check, State) ->
    check_active(),
    erlang:send_after(?ACTIVE_CHECK_DURATION, self(), circulation_active_check),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{circulation_active_check_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ets_create(PlayerID, Record) ->
    {Tab, _ValueList, Name, Key} = model_info(Record),
    put_record_status(PlayerID, Name, Key, create, undefined),
    true = ets:insert_new(Tab, Record).

ets_delete(PlayerID, SelectorRecord) ->
    {Tab, _ValueList, Name, Key} = model_info(SelectorRecord),
    if
        is_binary(Key) andalso Key =/= <<"">> ->
            put_record_status(PlayerID, Name, Key, delete, undefined),
            true = ets:delete(Tab, Key);
        true ->
            Recs = ets_where(SelectorRecord),
            lists:foreach(
                fun(Rec) ->
                    RecValues = tuple_to_list(Rec),
                    [RecName, RecKey|_Values] = RecValues,
                    put_record_status(PlayerID, RecName, RecKey, delete, undefined),
                    true = ets:delete(Tab, RecKey)
                end, Recs)
    end,
    true.

ets_update(PlayerID, SelectorRecord, ModifierRecord) ->
    {Tab, _ValueList, Name, Key} = model_info(SelectorRecord),
    ElementSpec = ets_utils:make_element_spec(ModifierRecord),
    RecordFields = record_mapper:get_mapping(Name),
    Fields = [lists:nth(FieldPosition - 1, RecordFields) || {FieldPosition, _Value} <- ElementSpec],
    case  id_present(Key) of
        true ->
            put_record_status_update(PlayerID, Name, Key, update, Fields),
            true = ets:update_element(Tab, Key, ElementSpec);
        false ->
            Recs = ets_where(SelectorRecord),
            lists:foreach(
                fun(Rec) ->
                    RecValues = tuple_to_list(Rec),
                    [RecName, RecKey|_Values] = RecValues,
                    put_record_status_update(PlayerID, RecName, RecKey, update, Fields),
                    ets:update_element(Tab, RecKey, ElementSpec)
                end, Recs)
    end,
    true.

ets_find(SelectorRecord) ->
    {Tab, _ValueList, _Name, Key} = model_info(SelectorRecord),
    case id_present(Key) of
        true ->
            case ets:lookup(Tab, Key) of
                [Rec] ->
                    Rec;
                [] ->
                    undefined
            end;
        false ->
            Pat = ets_utils:makepat(SelectorRecord),
            ets:match_object(Tab, Pat),
            case ets:match_object(Tab, Pat, 1) of
                {[Rec], _Continuation} ->
                    Rec;
                _ ->
                    undefined
            end
    end.

ets_where(SelectorRecord) ->
    {Tab, _, _, _} = model_info(SelectorRecord),
    Pat = ets_utils:makepat(SelectorRecord),
    ets:match_object(Tab, Pat).

ets_count(SelectorRecord) ->
    {Tab, _, _, _} = model_info(SelectorRecord),
    ets:select_count(Tab, [{ets_utils:makepat(SelectorRecord), [], [true]}]).

id_present(Key) when is_binary(Key) andalso Key =/= <<"">> ->
    true;
id_present(_Key) ->
    false.

model_info(Record) ->
    RecordValueList = tuple_to_list(Record),
    [RecordName, Key|_] = RecordValueList,
    Tab = table(RecordName),
    {Tab, RecordValueList, RecordName, Key}.

validate_ownership(PlayerID, FromPid) ->
    OwnerPid = ?GET_PID({player, PlayerID}),
    FromPid =:= OwnerPid.

track_active(PlayerID, Record) ->
    [ModelName|_] = tuple_to_list(Record),
    Key = {track_active, ModelName, PlayerID},
    Value = time_utils:current_time(),
    ets:insert(?STATE_TAB, {Key, Value}).

check_active() ->
    CurrentTime = time_utils:current_time(),
    lists:foreach(
        fun([ModelName, PlayerID, LastActive]) ->
            if
                (CurrentTime - LastActive) >= ?DATA_EXPIRE_DURATION ->
                    player:clean_data(PlayerID, ModelName);
                true ->
                    ok
            end
        end, active_specs()).

-spec(active_specs() -> [{ModelName::atom(), PlayerID::binary(), LastActive::integer()}]).
active_specs() ->
    ets:match(?STATE_TAB, {{track_active, '$1', '$2'}, '$3'}).

-spec(all_record_status() -> [{PlayerID::binary(), ModelName::atom(), Id::binary(), Status::atom(), Value::tuple()}]).
all_record_status() ->
    ets:match(?STATE_TAB, {{record_status, '$1', '$2', '$3'}, {'$4', '$5'}}).

%% Key = {ModelName, ID}, Status = delete|update|create, Value = undefined|ChangedFields
%% ChangedFields = [atom()]
put_record_status(PlayerID, ModelName, Id, Status, Value) ->
    Key = {record_status, PlayerID, ModelName, Id},
    Val = {Status, Value},
    true = ets:insert(?STATE_TAB, {Key, Val}).

get_record_status(PlayerID, ModelName, Id) ->
    Key = {record_status, PlayerID, ModelName, Id},
    case ets:lookup(?STATE_TAB, Key) of
        [{Key, Val}] ->
            Val;
        _ ->
            undefined
    end.

put_record_status_update(PlayerID, ModelName, Id, update, Fields) ->
    case get_record_status(PlayerID, ModelName, Id) of
        undefined ->
            put_record_status(PlayerID, ModelName, Id, update, Fields);
        {update, ChangedFields} ->
            Set = gb_sets:from_list(ChangedFields),
            NewFields = [Field || Field <- Fields, not gb_sets:is_element(Field, Set)],
            case NewFields =:= [] of
                true ->
                    ok;
                false ->
                    NewChangedFields = lists:flatten([NewFields|ChangedFields]),
                    put_record_status(PlayerID, ModelName, Id, update, NewChangedFields)
            end
    end.

%% Only can be invoked by the owner
del_record_status(PlayerID, ModelName, Id) ->
    Key = {record_status, PlayerID, ModelName, Id},
    true = ets:delete(?STATE_TAB, Key).

set_loaded(PlayerID, ModelName, Loaded) ->
    Key = {PlayerID, ModelName, loaded},
    if
        Loaded =:= true ->
            ets:insert(?STATE_TAB, {Key, true});
        true ->
            ets:delete(?STATE_TAB, Key)
    end.

get_loaded(PlayerID, ModelName) ->
    Key = {PlayerID, ModelName, loaded},
    case ets:lookup(?STATE_TAB, Key) of
        [{Key, Value}] ->
            Value;
        _ ->
            undefined
    end.

ensure_data_loaded(PlayerID, SelectorRecord) ->
    [ModelName|_] = tuple_to_list(SelectorRecord),
    case get_loaded(PlayerID, ModelName) of
        undefined ->
            player:load_data(PlayerID, ModelName);
        _ ->
            ok
    end.
