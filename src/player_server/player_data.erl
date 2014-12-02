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
         update/2,
         update/3,
         find/2,
         where/2,
         all/2,
         count/2,
         count_all/2,

         persist_for_player/1,
         get_all_recs/2,
         get_loaded_tables/1,
         ensure_load_data/2,
         update_rec_status/4,
         delete_rec_status/4,
         update_rec/2,
         delete_rec/3,
         create_rec/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/common_const.hrl").

-define(DATA_STATUS, player_ets_data_status).
-define(DATA_LOADED, player_ets_data_loaded).
-define(ACTIVE_CHECK_DURATION, 3600000). % 1 hour

-record(state, {circulation_active_check_timer}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Timer = erlang:send_after(?ACTIVE_CHECK_DURATION, self(), circulation_active_check),
    % {ok, #state{circulation_active_check_timer = Timer}}.
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{circulation_active_check_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Only can be invoked by game_connection
get_player_id(Udid) ->
    PlayerID = users_model:get_player_id(Udid),
    case application:get_env(game_server, server_environment) of
        {ok, test} -> player:proxy(PlayerID, model, persist_all, []);
        _ -> do_nothing
    end,
    PlayerID.

create(PlayerID, Record) when is_tuple(Record) ->
    case validate_ownership(PlayerID) of
        true -> model:create(Record);
        false -> player:proxy(PlayerID, model, create, [Record])
    end;
create(PlayerID, Records) when is_list(Records) ->
    case validate_ownership(PlayerID) of
        true ->
            lists:foreach(fun(Record) ->
                              model:create(Record)
                          end, Records);
        false ->
            lists:foreach(fun(Record) ->
                              player:proxy(PlayerID, model, create, [Record])
                          end, Records)
    end.

delete(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:delete(Selector);
        false -> player:proxy(PlayerID, model, delete, [Selector])
    end.

update(PlayerID, NewRecord) ->
    case validate_ownership(PlayerID) of
        true -> model:update(NewRecord);
        false -> player:proxy(PlayerID, model, update, [NewRecord])
    end.

update(PlayerID, Selector, Modifier) ->
    case validate_ownership(PlayerID) of
        true -> model:update(Selector, Modifier);
        false -> player:proxy(PlayerID, model, update, [Selector, Modifier])
    end.

find(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:find(Selector);
        false -> player:proxy(PlayerID, model, find, [Selector])
    end.

where(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:where(Selector);
        false -> player:proxy(PlayerID, model, where, [Selector])
    end.

all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:all(Table);
        false -> player:proxy(PlayerID, model, all, [Table])
    end.

count(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:count(Selector);
        false -> player:proxy(PlayerID, model, count, [Selector])
    end.

count_all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:count_all(Table);
        false -> player:proxy(PlayerID, model, count_all, [Table])
    end.

ensure_load_data(PlayerID, Table) ->
    EtsTab = ets_tab_name(Table),
    case get_loaded(PlayerID, Table) of
        true  -> true;
        false ->
            Module = list_to_atom(atom_to_list(Table) ++ "_model"),
            case Module:load_data(PlayerID) of
                {ok, []} -> undefined;
                {ok, Recs} -> insert_recs(PlayerID, Recs, Module, EtsTab)
            end,
            set_loaded(PlayerID, Table, true)
    end.

get_all_recs(PlayerID, Table) ->
    ensure_load_data(PlayerID, Table),
    Key = {PlayerID, Table},
    Tab = ets_tab_name(Table),
    lists:foldl(fun({_Key, {Id, Status}}, Result) ->
        if
            Status =:= ?MODEL_DELETE -> Result;
            true ->
                case ets:lookup(Tab, Id) of
                    [] -> Result;
                    [Rec] -> [Rec|Result]
                end
        end
    end, [], ets:lookup(?DATA_STATUS, Key)).

update_rec_status(PlayerID, Table, Id, Status) ->
    ets:insert(?DATA_STATUS, {{PlayerID, Table}, {Id, Status}}).

delete_rec_status(PlayerID, Table, Id, Status) ->
    ets:delete_object(?DATA_STATUS, {{PlayerID, Table}, {Id, Status}}).

clean_rec_status(PlayerID) ->
    lists:foreach(fun({_, Table}) ->
        ets:delete(?DATA_STATUS, {PlayerID, Table})
    end, get_loaded_tables(PlayerID)).

get_rec_status(PlayerID, Table) ->
    ets:lookup(?DATA_STATUS, {PlayerID, Table}).

update_rec(Table, Rec) ->
    EtsTab = ets_tab_name(Table),
    ets:insert(EtsTab, Rec).

delete_rec(PlayerID, Table, Id) ->
    EtsTab = ets_tab_name(Table),
    ets:delete(EtsTab, Id).

create_rec(PlayerID, Table, Rec) ->
    EtsTab = ets_tab_name(Table),
    insert_rec(PlayerID, Rec, EtsTab).

%%%===================================================================
%%% Internal functions
%%%===================================================================
validate_ownership(PlayerID) ->
    PlayerID =:= get(player_id).

get_loaded(PlayerID, ModelName) ->
    Key = {PlayerID, ModelName, loaded},
    case ets:lookup(?DATA_LOADED, Key) of
        [_] -> true;
        [] -> false
    end.

set_loaded(PlayerID, ModelName, Loaded) ->
    Key = {PlayerID, ModelName, loaded},
    if
        Loaded =:= true ->
            ets:insert(?DATA_LOADED, {Key, true}),
            ets:insert(?DATA_STATUS, {{PlayerID, loaded_tables}, ModelName});
        true ->
            ets:delete(?DATA_LOADED, Key)
    end.

get_loaded_tables(PlayerID) ->
    ets:lookup(?DATA_STATUS, {PlayerID, loaded_tables}).

insert_recs(PlayerID, Recs, Module, EtsTab) ->
    case lists:keyfind(serialize, 1, Module:module_info(attributes)) of
        false ->
            lists:foreach(fun(Rec) -> insert_rec(PlayerID, Rec, EtsTab) end, Recs);
        {serialize, Rule} ->
            Fields = record_mapper:get_mapping(hd(tuple_to_list(hd(Recs)))),
            lists:foreach(fun(Rec) -> 
                NewRec = deserialize(Rec, Fields, Rule),
                insert_rec(PlayerID, NewRec, EtsTab)
            end, Recs)
    end.

insert_rec(PlayerID, Rec, EtsTab) ->
    ets:insert(EtsTab, Rec),
    [Table, Id|_] = tuple_to_list(Rec),
    update_rec_status(PlayerID, Table, Id, ?MODEL_ORIGIN).

deserialize(Rec, Fields, Rule) ->
    [RecName|Values] = tuple_to_list(Rec),
    TermValues = deserialize(Values, Fields, Rule, []),
    list_to_tuple([RecName|TermValues]).

deserialize([], [], _Rule, Result) -> 
    lists:reverse(Result);
deserialize([Value|Values], [Field|Fields], Rule, Result) ->
    case lists:member(Field, Rule) of
        true when Value =/= undefined -> 
            TermValue = case base64:decode(Value) of
                <<>> -> undefined;
                Data -> binary_to_term(Data)
            end,
            deserialize(Values, Fields, Rule, [TermValue|Result]);
        _ ->
            deserialize(Values, Fields, Rule, [Value|Result])
    end.

ets_tab_name(Table) ->
    list_to_atom(atom_to_list(Table) ++ "_ets_table").

persist_for_player(PlayerID) ->
    try do_persist_for_player(PlayerID) of
        _Result -> 
            clean_rec_status(PlayerID),
            true
    catch
        Type:Msg ->
            exception:notify(Type, Msg),
            false
    end.

do_persist_for_player(PlayerID) ->
    case get_persist_all_sql(PlayerID) of
        <<>> -> do_nothing;
        JoinedSql -> 
            ProcedureName = db:procedure_name(<<"player">>, PlayerID),
            db:execute_with_procedure(ProcedureName, JoinedSql)
    end.

get_persist_all_sql(PlayerID) ->
    logger:info("PERSIST FOR: ~p~n", [PlayerID]),
    logger:info("loaded tables: ~p~n", [get_loaded_tables(PlayerID)]),
    Sqls = lists:foldl(fun({_, Table}, Result) ->
        case generate_persist_sql(PlayerID, Table) of
            <<>> -> Result;
            Sql -> 
                logger:info("[~p] Table: ~p SQL: ~p~n", [PlayerID, Table, Sql]),
                [Sql|Result]
        end
    end, [], get_loaded_tables(PlayerID)),
    binary_string:join(Sqls, <<";">>).

generate_persist_sql(PlayerID, Table) ->
    Tab = ets_tab_name(Table),
    logger:info("table: ~p, rec status: ~p~n", [Table, get_rec_status(PlayerID, Table)]),
    Sqls = lists:foldl(fun({_, {Id, Status}}, Result) ->
        if
            Status =:= ?MODEL_ORIGIN ->
                Result;
            Status =:= ?MODEL_CREATE orelse Status =:= ?MODEL_UPDATE ->
                [Rec] = ets:lookup(Tab, Id),
                [sql(Rec, Status)|Result];
            Status =:= ?MODEL_DELETE ->
                [sql({Table, Id}, Status)|Result]
        end
    end, [], get_rec_status(PlayerID, Table)),
    case Sqls of
        [] -> <<>>;
        _ -> binary_string:join(Sqls, <<";">>)
    end.

sql(Rec, ?MODEL_CREATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    db_fmt:format("INSERT INTO `~s` (~s) VALUES (~s)", 
                  [Table, join_fields(Fields), join_values(Values)]);
sql(Rec, ?MODEL_UPDATE) ->
    {Table, Fields, Values} = rec_info(Rec),
    Uuid = hd(Values),
    db_fmt:format("UPDATE `~s` SET ~s WHERE `uuid` = ~s", 
                  [Table, db_fmt:map(Fields, Values), db_fmt:encode(Uuid)]);
sql({Table, Uuid}, ?MODEL_DELETE) ->
    db_fmt:format("DELETE FROM `~s` WHERE `uuid` = ~s", 
                  [Table, db_fmt:encode(Uuid)]).

rec_info(Rec) ->
    [Table|Values] = tuple_to_list(Rec),
    Fields = record_mapper:get_mapping(Table),
    Module = list_to_atom(atom_to_list(Table) ++ "_model"),
    Rule = proplists:get_value(serialize, Module:module_info(attributes)),
    SerializedValues = serialize(Values, Fields, Rule),
    {Table, Fields, SerializedValues}.

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

join_fields(Fields) ->
    join_fields(Fields, []).
join_fields([], Result) ->
    binary_string:join(lists:reverse(Result), <<", ">>);
join_fields([Field|Fields], Result) ->
    NewField = list_to_binary([<<"`">>, atom_to_binary(Field, utf8), <<"`">>]),
    join_fields(Fields, [NewField|Result]).

join_values(Values) ->
    join_values(Values, []).
join_values([], Result) ->
    binary_string:join(lists:reverse(Result), <<", ">>);
join_values([Value|Values], Result) -> 
    join_values(Values, [db_fmt:encode(Value)|Result]).
