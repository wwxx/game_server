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
-export ([start_link/0,
          create/1,
          delete_by/3,
          delete_all/1,
          update_by/3,
          find_by/3,
          find_by/4,
          request/2,
          all/1,
          update_all/1,
          sqerl_execute/1,
          execute/1,
          procedure_name/2,
          execute_with_procedure/2,
          clean_all_procedures/0
         ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include ("include/db_schema.hrl").
-include ("include/db_config.hrl").

-define (DB_POOL, database_pool).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

create(Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    sqerl_execute({insert, TableName, map(Fields, Values)}).

delete_by(TableName, Field, Value) ->
    sqerl_execute({delete, TableName, {Field, '=', Value}}).

delete_all(TableName) ->
    sqerl_execute({delete, TableName}).

update_by(Field, Value, Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    sqerl_execute({update, TableName, map(Fields, Values), {where, {Field, '=', Value}}}).

update_all(Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    sqerl_execute({update, TableName, map(Fields, Values)}).

find_by(TableName, Field, Value) ->
    SqlTuple = {select, '*', {from, TableName}, {where, {Field, '=', Value}}},
    request(TableName, SqlTuple).

find_by(TableName, Field, Value, {fields, SelectFields}) ->
    SqlTuple = {select, SelectFields, {from, TableName}, {where, {Field, '=', Value}}},
    request(TableName, SqlTuple);
find_by(TableName, Field, Value, {limit, Limit}) ->
    SqlTuple = {select, '*', {from, TableName}, {where, {Field, '=', Value}}, {limit, Limit}},
    request(TableName, SqlTuple).

all(TableName) ->
    SqlTuple = {select, '*', {from, TableName}},
    request(TableName, SqlTuple).

request(TableName, SqlTuple) ->
    Res = sqerl_execute(SqlTuple),
    Fields = record_mapper:get_mapping(TableName),
    {ok, emysql_util:as_record(Res, TableName, Fields)}.

sqerl_execute(SqlTuple) ->
    execute(sqerl:sql(SqlTuple)).

clean_all_procedures() ->
    execute(<<"DELETE FROM mysql.proc;">>).

procedure_name(Name, Suffix) ->
    list_to_binary([Name, <<"_">>, Suffix]).

execute_with_procedure(ProcedureName, Sql) ->
    DropProcedure = list_to_binary([<<"DROP PROCEDURE IF EXISTS ">>, ProcedureName]),
    CreateProcedure = list_to_binary([<<"CREATE PROCEDURE ">>, ProcedureName, <<"()">>,
                                      <<" BEGIN ">>,
                                      <<" DECLARE exit handler for sqlexception ">>,
                                      <<" BEGIN ">>,
                                      <<" ROLLBACK; ">>,
                                      <<" RESIGNAL; ">>,
                                      <<" END; ">>,
                                      <<" DECLARE exit handler for sqlwarning ">>,
                                      <<" BEGIN ">>,
                                      <<" ROLLBACK; ">>,
                                      <<" RESIGNAL; ">>,
                                      <<" END; ">>,
                                      <<" START TRANSACTION; ">>,
                                      Sql, <<";">>,
                                      <<" COMMIT; ">>,
                                      <<" END ">>]),
    ExecuteProcedure = list_to_binary([<<"CALL ">>, ProcedureName, <<"();">>]),
    %error_logger:info_msg("Create: ~p~n", [CreateProcedure]),
    %% clean old procedure
    db:execute(DropProcedure),
    %% create procedure for palyer's current state
    db:execute(CreateProcedure),
    %% call procedure
    db:execute(ExecuteProcedure),
    %% clean procedure
    db:execute(DropProcedure).

%%--------------------------------------------------------------------
%% @doc:    Execute SQL and return Result
%% @spec:    execute(SQL::binary()) -> list().
%% @end
%%--------------------------------------------------------------------
-spec(execute(binary()) -> list() ).
execute(SQL) ->
    Result = emysql:execute(?DB_POOL, SQL),
    %error_logger:info_msg("SQL EXECUTE RESULT: ~p~n", [Result]),
    case is_tuple(Result) of
        true ->
            case tuple_to_list(Result) of
                [error_packet|_] ->
                    erlang:error(Result);
                _ ->
                    Result
            end;
        false ->
            Result
    end.

%%%===================================================================
%%% gen_server API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    init_pool(),
    clean_all_procedures(),
    {ok, #state{}}.

handle_call(init_pool, _From, State) ->
    io:format("Init Pool.~n"),
    Pool = init_pool(),
    {reply, Pool, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    remove_pool(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_pool() ->
    L = case application:get_env(game_server, server_environment) of
            {ok, production} -> ?DB_PRODUCTION;
            {ok, development} -> ?DB_DEVELOPMENT;
            {ok, test} -> ?DB_TEST
        end,
    Database = atom_to_list(proplists:get_value(database, L)),
    Username = atom_to_list(proplists:get_value(username, L)),
    Password = atom_to_list(proplists:get_value(password, L)),
    Encoding = proplists:get_value(encoding, L),
    PoolSize = proplists:get_value(pool,L),
    ok = emysql:add_pool(?DB_POOL, PoolSize, Username, Password,
                         "localhost", 3306, Database, Encoding).

remove_pool() ->
    ok = emysql:remove_pool(?DB_POOL).


map(Fields, Values) ->
    map(Fields, Values, []).

map([], [], Result) ->
    Result;
map([_Field|Fields], [Value|Values], Result) when Value =:= undefined ->
    map(Fields, Values, Result);
map([Field|Fields], [Value|Values], Result) ->
    map(Fields, Values, [{Field, Value}|Result]).
