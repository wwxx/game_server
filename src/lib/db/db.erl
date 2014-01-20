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
          test_mongo/0,
          delete_one/1,
          delete_all/1,
          replace/2,
          update/2,
          modify/3,
          replace_or_insert/2,
          find_one/1,
          find_one/2,
          find_one/3,
          where/1,
          where/2,
          where/3,
          where/4,
          count/1,
          count/2,
          execute/1,
          add_mappings/0,
          get_father_info/1,
          binary_string_to_objectid/1,
          objectid_to_binary_string/1,
          test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define (DB_POOL, mongodb_pool).
-define (DB, civilation_production).

-define(FATHER_INFO, [
        {buffs, {towns, town_id}},
        {act_reinforces, {activities, activity_id}},
        {bookmarks, {users, user_id}},
        {heros, {users, user_id}},
        {researches, {users, user_id}},
        {tasks, {users, user_id}}]).


-include ("include/db_schema.hrl").

-define(id(), '_id'=mongodb_app:gen_objectid()).
-define(mapping(Record), {Record, record_info(fields, Record)}).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
test() ->
    %% 创建记录
    {ok, Id} = create(#users{?id(), user_name = <<"Savin">>}),
    %% 高级查询
    {ok, Recs} = where({'$orderby', #users{user_name=1}, '$query', #users{user_name= {regex, <<"t">>, <<"i">>}}}, #users{user_name=1, '_id'=0}),
    {ok, User} = find_one(#users{user_name= <<"Savin">>}),
    %% 替换整个Document
    replace(User, User#users{user_name = <<"Savin-Max-By-Replace">>}),
    %% 修改某些字段
    update(User, User#users{user_name = <<"Savin-Max-By-Update">>}),
    % mongrel:replace(User, Book#book{reviews = [#review{comment= <<"The well known Dutch author surpasses himself!">>}]}),
    io:format("Recs: ~p~n", [Recs]),
    find_one(#users{'_id'=Id}).

get_father_info(RecordName) ->
    proplists:get_value(RecordName, ?FATHER_INFO).

create(Record) when is_tuple(Record)  ->
    execute(fun() -> mongrel:insert(Record) end);
create(Records) when is_list(Records)  ->
    execute(fun() -> mongrel:insert_all(Records) end).

delete_one(SelectorRecord) ->
    execute(fun() -> mongrel:delete_one(SelectorRecord) end).

delete_all(SelectorRecord) ->
    execute(fun() -> mongrel:delete(SelectorRecord) end).

replace(SelectorRecord, NewRecord) ->
    execute(fun() -> mongrel:replace(SelectorRecord, NewRecord) end).

update(SelectorRecord, ModifierRecord) ->
    modify(SelectorRecord, '$set', ModifierRecord).

mongo_modify(Collection, Selector, Modifier) ->
    mongo_execute(fun() -> mongo:modify(Collection, Selector, Modifier) end).

test_mongo() ->
    ID = {<<81,173,239,23,17,135,53,49,254,0,6,199>>},
    mongo_modify(users, {'heros._id', ID}, {'$pull', {heros, {'_id',ID}}}).


%% Avaialbe Modifiers:
%% $set
%% $unset
%% $inc
%% $push
%% $pop
%% $pull
%% $each
%% $addToSet
modify(SelectorRecord, Modifier, ModifierRecord) ->
    execute(fun() -> mongrel:modify(SelectorRecord, {Modifier, ModifierRecord}) end).

replace_or_insert(SelectorRecord, NewRecord) ->
    execute(fun() -> mongrel:repsert(SelectorRecord, NewRecord) end).

find_one(SelectorRecord) ->
    execute(fun() -> mongrel:find_one(SelectorRecord) end).

find_one(SelectorRecord, ProjectorRecord) ->
    execute(fun() -> mongrel:find_one(SelectorRecord, ProjectorRecord) end).

find_one(SelectorRecord, ProjectorRecord, Skip) ->
    execute(fun() -> mongrel:find_one(SelectorRecord, ProjectorRecord, Skip) end).

where(SelectorRecord) ->
    execute(fun() ->
                Cursor = mongrel:find(SelectorRecord),
                mongrel_cursor:rest(Cursor)
            end).

where(SelectorRecord, ProjectorRecord) ->
    io:format("SelectorRecord: ~p~n", [SelectorRecord]),
    io:format("ProjectorRecord: ~p~n", [ProjectorRecord]),
    execute(fun() ->
                Cursor = mongrel:find(SelectorRecord, ProjectorRecord),
                mongrel_cursor:rest(Cursor)
            end).

where(SelectorRecord, ProjectorRecord, Skip) ->
    execute(fun() ->
                Cursor = mongrel:find(SelectorRecord, ProjectorRecord, Skip),
                mongrel_cursor:rest(Cursor)
            end).

where(SelectorRecord, ProjectorRecord, Skip, BatchSize) ->
    execute(fun() ->
                Cursor = mongrel:find(SelectorRecord, ProjectorRecord, Skip, BatchSize),
                mongrel_cursor:rest(Cursor)
            end).

count(SelectorRecord) ->
    execute(fun() -> mongrel:count(SelectorRecord) end).

count(SelectorRecord, Limit) ->
    execute(fun() -> mongrel:count(SelectorRecord, Limit) end).

execute(Action) ->
    mongrel:do(safe, master, conn(), ?DB, Action).

mongo_execute(Action) ->
    mongo:do(safe, master, conn(), ?DB, Action).

%%--------------------------------------------------------------------
%% @doc:    Generate ObjectId from Binary String
%% @spec:    binary_string_to_objectid(BinaryString::binary()) -> {binary()}.
%% @end
%%--------------------------------------------------------------------
-spec(binary_string_to_objectid(binary()) -> {binary()}).
binary_string_to_objectid(BinaryString) ->
    binary_string_to_objectid(BinaryString, []).

binary_string_to_objectid(<<>>, Result) ->
    {list_to_binary(lists:reverse(Result))};
binary_string_to_objectid(<<BS:2/binary, Bin/binary>>, Result) ->
    binary_string_to_objectid(Bin, [erlang:binary_to_integer(BS, 16)|Result]).

%%--------------------------------------------------------------------
%% @doc:    Generate Binary String from ObjectId
%% @spec:    objectid_to_binary_string(ObjectId::{binary()}) -> binary().
%% @end
%%--------------------------------------------------------------------
-spec(objectid_to_binary_string({binary()}) -> binary()).
objectid_to_binary_string({Id}) ->
    objectid_to_binary_string(Id, []).

objectid_to_binary_string(<<>>, Result) ->
    list_to_binary(lists:reverse(Result));
objectid_to_binary_string(<<Hex:8, Bin/binary>>, Result) ->
    StringList1 = erlang:integer_to_list(Hex, 16),
    StringList2 = case erlang:length(StringList1) of
        1 ->
            ["0"|StringList1];
        _ ->
            StringList1
    end,
    objectid_to_binary_string(Bin, [StringList2|Result]).

%%%===================================================================
%%% gen_server API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    init_pool(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(init_pool, _From, State) ->
    io:format("Init Pool.~n"),
    Pool = init_pool(),
    {reply, Pool, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({stop_pool, Pool}, State) ->
    case resource_pool:is_closed(Pool) of
        true -> ok;
        false -> resource_pool:close(Pool)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
conn() ->
    Pool = pool(),
    case resource_pool:get(Pool) of
        {ok, Conn} ->
            Conn;
        {error, Reason} ->
            io:format("Get MongoDb Connection Failed: ~p~n", [Reason]),
            stop_pool(Pool),
            conn()
    end.

pool() ->
    case cached_pool() of
        {alive, Pool} ->
            Pool;
        _ ->
            gen_server:call(?SERVER, init_pool)
    end.

-spec(cached_pool() -> undefined|{alive|dead, Pool::any()}).
cached_pool() ->
    case game_env:get(?DB_POOL) of
        undefined -> undefined;
        Pool -> {pool_status(Pool), Pool}
    end.

pool_status(Pool) ->
    case resource_pool:is_closed(Pool) of
        true -> dead;
        false -> alive
    end.

init_pool() ->
    case cached_pool() of
        {alive, CachedPool} ->
            CachedPool;
        _ ->
            NewPool = resource_pool:new(mongo:connect_factory(localhost), 8),
            game_env:set(?DB_POOL, NewPool),
            add_mappings(),
            NewPool
    end.

stop_pool(Pool) ->
    gen_server:cast(?SERVER, {stop_pool, Pool}).

add_mappings() ->
    add_mapping(?mapping(act_reinforces)),
    add_mapping(?mapping(activities)),
    add_mapping(?mapping(alliances)),
    add_mapping(?mapping(alliance_applications)),
    add_mapping(?mapping(alliance_bookmarks)),
    add_mapping(?mapping(alliance_events)),
    add_mapping(?mapping(alliance_researches)),
    add_mapping(?mapping(black_udids)),
    add_mapping(?mapping(blacklists)),
    add_mapping(?mapping(bookmarks)),
    add_mapping(?mapping(buffs)),
    add_mapping(?mapping(competitions)),
    add_mapping(?mapping(device_maps)),
    add_mapping(?mapping(diplomacies)),
    add_mapping(?mapping(events)),
    add_mapping(?mapping(friend_lists)),
    add_mapping(?mapping(heros)),
    add_mapping(?mapping(iaps)),
    add_mapping(?mapping(injured_armies)),
    add_mapping(?mapping(invitations)),
    add_mapping(?mapping(items)),
    add_mapping(?mapping(letters)),
    add_mapping(?mapping(researches)),
    add_mapping(?mapping(tasks)),
    add_mapping(?mapping(towns)),
    add_mapping(?mapping(town_reinforces)),
    add_mapping(?mapping(users)),
    add_mapping(?mapping(user_competitions)).

add_mapping({RecordName, Fields}) ->
    Mapping = {RecordName, Fields},
    record_mapper:add_mapping(Mapping),
    mongrel_mapper:add_mapping(Mapping).


%%%===================================================================
%%% Internal functions
%%%===================================================================


