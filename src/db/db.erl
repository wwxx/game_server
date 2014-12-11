%%%-------------------------------------------------------------------
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
%%%        Hold DB pool.
%%%        Provide DB API.
%%% @end
%%% Created :  六 10 12 14:42:45 2013 by Savin-Max
%%%-------------------------------------------------------------------
-module (db).

-behaviour(gen_server).

%% API
-export ([start_link/1,
          init_pool/1,
          create/1,
          delete_by/3,
          delete_all/1,
          update_by/3,
          count_by/3,
          find_by/3,
          find_by/4,
          request/2,
          all/1,
          update_all/1,
          execute/1,
          procedure_name/2
         ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (DB_POOL, database_pool).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

create(Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    Sql = db_fmt:format("INSERT INTO `~s` (~s) VALUES (~s)", 
                        [TableName, db_fmt:map(Fields, Values)]),
    execute(Sql).

delete_by(TableName, Field, Value) ->
    Sql = db_fmt:format("DELETE FROM `~s` WHERE `~s` = ~s", 
                        [TableName, Field, db_fmt:encode(Value)]),
    execute(Sql).

delete_all(TableName) ->
    execute(db_fmt:format("DELETE FROM `~s`", [TableName])).

update_by(Field, Value, Record) ->
    [Table|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Table),
    Sql = db_fmt:format("UPDATE `~s` SET ~s WHERE `~s` = ~s", 
                        [Table, db_fmt:map(Fields, Values), Field, db_fmt:encode(Value)]),
    execute(Sql).

update_all(Record) ->
    [Table|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(Table),
    Sql = db_fmt:format("UPDATE `~s` SET ~s", [Table, db_fmt:map(Fields, Values)]),
    execute(Sql).

count_by(Table, Field, Value) ->
    Sql = db_fmt:format("SELECT count(*) FROM `~s` WHERE `~s` = ~s", 
                        [Table, Field, db_fmt:encode(Value)]),
    execute(Sql).

%% Sqerl = {Field, '=', Value}
%% Sqerl = {{Field, '=', Value}, 'or', {FieldB, '=', ValueB}}
% where(TableName, Sqerl) ->
%     SqlTuple = {select, '*', {from, TableName}, {where, Sqerl}},
%     request(TableName, SqlTuple).

find_by(Table, Field, Value) ->
    Sql = db_fmt:format("SELECT * FROM `~s` WHERE `~s` = ~s", 
                        [Table, Field, db_fmt:encode(Value)]),
    request(Table, Sql).

%% OrderArgs: {created_at, desc}
find_by(Table, Field, Value, {order_by, {OrderField, Arrow}}) ->
    Sql = db_fmt:format("SELECT * FROM `~s` WHERE `~s` = ~s ORDER BY ~s ~s", 
                        [Table, Field, db_fmt:encode(Value), OrderField, Arrow]),
    request(Table, Sql);
find_by(Table, Field, Value, {fields, SelectFields}) ->
    Sql = db_fmt:format("SELECT ~s FROM `~s` WHERE `~s` = ~s",
                        [db_fmt:join_fields(SelectFields), Table, 
                         Field, db_fmt:encode(Value)]),
    request(Table, Sql);
find_by(Table, Field, Value, {limit, Limit}) ->
    Sql = db_fmt:format("SELECT * FROM `~s` WHERE `~s` = ~s LIMIT ~s",
                        [Table, Field, db_fmt:encode(Value), Limit]),
    request(Table, Sql).

all(Table) ->
    Sql = db_fmt:format("SELECT * FROM `~s`", [Table]),
    request(Table, Sql).

request(TableName, Sql) ->
    Fields = record_mapper:get_mapping(TableName),
    R = case execute(Sql) of
        {data, {mysql_result, _FieldDefines, [], _, _}} -> {ok, []};
        {data, {mysql_result, FieldDefines, Values, _, _}} ->
            Recs = lists:foldl(fun(Value, Result) ->
                [as_record(TableName, Fields, FieldDefines, Value)|Result]
            end, [], Values),
            case Recs of
                [] -> {ok, []};
                _ ->  {ok, lists:reverse(Recs)}
            end
    end,
    logger:info("DB REQUEST: ~p~n", [R]),
    R.

as_record(TableName, Fields, FieldDefines, Values) ->
    VS = lists:zipwith(fun({_, Field, _, _}, Value) ->
        {list_to_atom(Field), Value}
    end, FieldDefines, Values),
    R = lists:foldl(fun(Field, Result) ->
        case lists:keyfind(Field, 1, VS) of
            false -> [undefined|Result];
            {_Field, Value} -> [Value|Result]
        end
    end, [TableName], Fields),
    list_to_tuple(lists:reverse(R)).

cast_rows(FieldDefines, Rows) ->
    NewRows = lists:foldl(fun(Values, Result) ->
        [cast_row(FieldDefines, Values)|Result]
    end, [], Rows),
    lists:reverse(NewRows).

cast_row(FieldDefines, Values) ->
    lists:zipwith(fun({_, _, _, Type}, Value) ->
        cast_field_value(Type, Value)
    end, FieldDefines, Values).

cast_field_value(_Type, null) -> undefined;
cast_field_value(Type, Value) ->
        if
            Type =:= 'VAR_STRING' orelse Type =:= 'BLOB' ->
                list_to_binary(Value);
            Type =:= 'LONG' orelse Type =:= 'TINY' ->
                list_to_integer(Value);
            Type =:= 'FLOAT' ->
                case string:to_float(Value) of
                    {error, no_float} -> 
                        list_to_integer(Value);
                    Res -> Res
                end;
            Type =:= 'DATETIME' ->
                time_utils:to_timestamp(Value);
            true ->
                error(list_to_binary(io_lib:format("UNPARSED TYPE: ~p, Value: ~p", [Type, Value])))
        end.

procedure_name(Name, Suffix) ->
    list_to_binary([Name, <<"_">>, Suffix]).

%%--------------------------------------------------------------------
%% @doc:    Execute SQL and return Result
%% @spec:    execute(SQL::binary()) -> list().
%% @end
%%--------------------------------------------------------------------
-spec(execute(binary()) -> tuple() ).
execute(SQL) ->
    % error_logger:info_msg("SQL: ~p~n", [SQL]),
    Result = mysql:fetch(?DB_POOL, binary_to_list(SQL)),
    % error_logger:info_msg("SQL EXECUTE RESULT: ~p~n", [Result]),
    case is_tuple(Result) of
        true ->
            case tuple_to_list(Result) of
                [error_packet|_] ->
                    erlang:error(Result);
                _ ->
                    logger:info("Result: ~p~n", [Result]),
                    case Result of
                        {updated, V} -> {updated, V};
                        {data, {mysql_result, FieldDefines, [], A, B}} -> 
                            {data, {mysql_result, FieldDefines, [], A, B}};
                        {data, {mysql_result, FieldDefines, Values, A, B}} ->
                            CastedValues = cast_rows(FieldDefines, Values),
                            {data, {mysql_result, FieldDefines, CastedValues, A, B}}
                    end
            end;
        false ->
            Result
    end.

%%%===================================================================
%%% gen_server API
%%%===================================================================
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

init_pool(Config) ->
    gen_server:call(?SERVER, {init_pool, Config}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([_Config]) ->
    {ok, #state{}}.

handle_call({init_pool, Config}, _From, State) ->
    Pool = do_init_pool(Config),
    {reply, Pool, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init_pool(L) ->
    Database = atom_to_list(proplists:get_value(database, L)),
    Username = atom_to_list(proplists:get_value(username, L)),
    Password = atom_to_list(proplists:get_value(password, L)),
    PoolSize = proplists:get_value(pool,L),
    supervisor:start_child(db_sup, [?DB_POOL, "localhost", Username, Password, Database]),
    lists:foreach(fun(_) ->
        mysql:connect(?DB_POOL, "localhost", 3306, Username, Password, Database, true)
    end, lists:duplicate(PoolSize, 1)).
