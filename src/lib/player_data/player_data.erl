%%% $Key: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
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
         get_single_record_status/3,
         get_player_record_status/2,
         get_player_records_status/1,
         get_data_status/1,
         set_data_status/2,
         del_record_status/3,
         get_loaded/2,
         set_loaded/3
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
            RecordWithId = record_mapper:set_field(Record, '_id', NewId),
            ets_create(PlayerID, RecordWithId)
    end.

delete(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    case validate_ownership(PlayerID, self()) of
        true ->
            ets_delete(PlayerID, SelectorRecord)
    end.

update(PlayerID, SelectorRecord, ModifierRecord) ->
    track_active(PlayerID, SelectorRecord),
    case validate_ownership(PlayerID, self()) of
        true ->
            ets_update(PlayerID, SelectorRecord, ModifierRecord)
    end.

find(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    [ModelName|_] = tuple_to_list(SelectorRecord),
    case get_loaded(PlayerID, ModelName) of
        undefined ->
            player:load_data(PlayerID, ModelName);
        _ ->
            ok
    end,
    ets_find(SelectorRecord).

where(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    [ModelName|_] = tuple_to_list(SelectorRecord),
    case get_loaded(PlayerID, ModelName) of
        undefined ->
            player:load_data(PlayerID, ModelName);
        _ ->
            ok
    end,
    ets_where(SelectorRecord).

count(PlayerID, SelectorRecord) ->
    track_active(PlayerID, SelectorRecord),
    [ModelName|_] = tuple_to_list(SelectorRecord),
    case get_loaded(PlayerID, ModelName) of
        undefined ->
            player:load_data(PlayerID, ModelName);
        _ ->
            ok
    end,
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
                    record_mapper:set_field(Record, '_id', PlayerID);
                true ->
                    record_mapper:set_field(Record, user_id, PlayerID)
            end,
            true = ets:match_delete(Tab, ets_utils:makepat(SelectorRecord))
    end.

-spec(get_single_record_status(PlayerID::binary(), ModelName::atom(), Id::any()) ->
      [{Status::atom(), Value::[atom()]|undefined}]).
get_single_record_status(PlayerID, ModelName, Id) ->
    get_data_status({record_status, PlayerID, ModelName, Id}).

-spec(get_player_record_status(PlayerID::binary(), ModelName::atom()) ->
      [{Id::any(), Status::atom, Value::[atom()]|undefined}]).
get_player_record_status(PlayerID, ModelName) ->
    ets:match(?STATE_TAB, {{record_status, PlayerID, ModelName, '%2'}, '$3', '$4'}).

-spec(get_player_records_status(PlayerID::binary()) ->
      [{ModelName::atom(), Id::any(), Status::atom, Value::[atom()]|undefined}]).
get_player_records_status(PlayerID) ->
    ets:match(?STATE_TAB, {{record_status, PlayerID, '$1', '%2'}, '$3', '$4'}).

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
    {Tab, ValueList, Key, Name} = model_info(Record),
    put_record_status(PlayerID, Name, Key, create, undefined),
    true = ets:insert_new(Tab, ValueList).

ets_delete(PlayerID, SelectorRecord) ->
    {Tab, _ValueList, Key, Name} = model_info(SelectorRecord),
    if
        is_binary(Key) andalso Key =/= <<"">> ->
            put_record_status(PlayerID, Name, Key, delete, undefined),
            true = ets:delete(Tab, Key);
        true ->
            %true = ets:match_delete(Tab, ets_utils:makepat(SelectorRecord))
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
    {Tab, _ValueList, Key, Name} = model_info(SelectorRecord),
    ElementSpec = ets_utils:make_element_spec(ModifierRecord),
    Fields = [Field || {Field, _Value} <- ElementSpec],
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
    ets:select_count(Tab, ets_utils:makepat(SelectorRecord)).

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
            NewFields = [Field || Field <- ChangedFields,
                                  not gb_sets:is_element(Field, Set)],
            put_record_status(PlayerID, ModelName, Id, update, [NewFields|ChangedFields])
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
