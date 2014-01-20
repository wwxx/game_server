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
          execute/1,
          test_sql/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (DB_POOL, database_pool).

-include ("include/db_schema.hrl").

-define(id(), '_id'=mongodb_app:gen_objectid()).
-define(mapping(Record), {Record, record_info(fields, Record)}).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%test() ->
    %%% 创建记录
    %{ok, Id} = create(#users{?id(), user_name = <<"Savin">>}),
    %%% 高级查询
    %{ok, Recs} = where({'$orderby', #users{user_name=1}, '$query', #users{user_name= {regex, <<"t">>, <<"i">>}}}, #users{user_name=1, '_id'=0}),
    %{ok, User} = find_one(#users{user_name= <<"Savin">>}),
    %%% 替换整个Document
    %replace(User, User#users{user_name = <<"Savin-Max-By-Replace">>}),
    %%% 修改某些字段
    %update(User, User#users{user_name = <<"Savin-Max-By-Update">>}),
    %% mongrel:replace(User, Book#book{reviews = [#review{comment= <<"The well known Dutch author surpasses himself!">>}]}),
    %io:format("Recs: ~p~n", [Recs]),
    %find_one(#users{'_id'=Id}).

test_sql() ->
    execute(<<"select hello_text from hello_table">>).

%%--------------------------------------------------------------------
%% @doc:    Execute SQL and return Result
%% @spec:    execute(SQL::binary()) -> list().
%% @end
%%--------------------------------------------------------------------
-spec(execute(binary()) -> list() ).
execute(SQL) ->
    Result = emysql:execute(?DB_POOL, SQL),
    io:format("SQL Result: ~p~n", [Result]),
    Result.

as_records(RecordName, Fields, Result) ->
    emysql_util:as_record(Result, RecordName, Fields).

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

