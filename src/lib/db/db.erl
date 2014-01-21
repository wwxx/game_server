%%%-------------------------------------------------------------------
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) 2013 Savin-Max. All Rights Reserved.
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
          update_by/3,
          find_by/3,
          sqerl_execute/1,
          execute/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include ("include/db_schema.hrl").

-define (DB_POOL, database_pool).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

create(Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    execute(sqerl:sql({insert, TableName, map(Fields, Values)})).

delete_by(TableName, Field, Value) ->
    execute(sqerl:sql({delete, TableName, {Field, '=', Value}})).

update_by(Field, Value, Record) ->
    [TableName|Values] = tuple_to_list(Record),
    Fields = record_mapper:get_mapping(TableName),
    execute(sqerl:sql({update, TableName, map(Fields, Values), {where, {Field, '=', Value}}})).

find_by(TableName, Field, Value) ->
    Res = execute(sqerl:sql({select, '*', {from, TableName}, {where, {Field, '=', Value}}})),
    Fields = record_mapper:get_mapping(TableName),
    {ok, emysql_util:as_record(Res, TableName, Fields)}.

sqerl_execute(SqlTuple) ->
    execute(sqerl:sql(SqlTuple)).

%%--------------------------------------------------------------------
%% @doc:    Execute SQL and return Result
%% @spec:    execute(SQL::binary()) -> list().
%% @end
%%--------------------------------------------------------------------
-spec(execute(binary()) -> list() ).
execute(SQL) ->
    emysql:execute(?DB_POOL, SQL).

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
    [[_,_,{"production", L}]] = yamerl_constr:file("config/database.yml"),
    Database = proplists:get_value("database", L),
    Username = proplists:get_value("username", L),
    Password = proplists:get_value("password", L),
    Encoding = list_to_atom(proplists:get_value("encoding", L)),
    PoolSize = proplists:get_value("pool",L),
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
