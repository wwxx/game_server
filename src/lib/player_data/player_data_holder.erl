%%%-------------------------------------------------------------------
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) 2013 Savin-Max. All Rights Reserved.
%%% @doc
%%%        Hold ets tables
%%% @end
%%% Created :  五 10 11 00:27:26 2013 by Savin-Max
%%%-------------------------------------------------------------------
-module(player_data_holder).

-behaviour(gen_server).

%% API
-export([start_link/0, table/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

table(ModelName) ->
    Name = list_to_atom("game_server_model_" ++ atom_to_list(ModelName)),
    case get_table_created(Name) of
        true ->
            Name;
        _ ->
            Name = gen_server:call(?MODULE, {create_table, Name})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    player_data_state_tracker = ets:new(player_data_state_tracker,
        [set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, #state{}}.

handle_call({create_table, Name}, _From, State) ->
    case ets:info(Name) of
        undefined ->
            Name = ets:new(Name, ets_opts()),
            set_table_created(Name, true),
            Name;
        _ ->
            Name
    end,
    {reply, Name, State};
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

ets_opts() ->
    [set, public, named_table, {keypos, 2},
     {read_concurrency, true}, {write_concurrency, true}].

set_table_created(TableName, Value) ->
    erlang:put({TableName, table_created}, Value).

get_table_created(TableName) ->
    erlang:get({TableName, table_created}).
