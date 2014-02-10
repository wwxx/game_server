-module(player).

-behaviour(gen_server).

%% API
-export([start_link/1,
         request/3,
         send_data/2,
         load_data/2,
         clean_data/2,
         player_pid/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(player_state, {playerID, circulation_persist_timer}).

-define(PERSIST_DURATION, 1800000). %% 30 minutes

-include("include/gproc_macros.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_link(PlayerID) ->
    gen_server:start_link(?MODULE, [PlayerID], []).

request(PlayerID, Path, Params) ->
    gen_server:call(player_pid(PlayerID), {request, Path, Params}).

load_data(PlayerID, ModelName) ->
    gen_server:call(player_pid(PlayerID), {load_data, ModelName}).

clean_data(PlayerID, ModelName) ->
    gen_server:cast(player_pid(PlayerID), {clean_data, ModelName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerID]) ->
    io:format("playerID: ~p~n", [PlayerID]),
    ?REG_PID({player, PlayerID}),
    Timer = erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    {ok, #player_state{playerID=PlayerID, circulation_persist_timer = Timer}}.

handle_call({request, {Controller, Action}, Params}, _From,
            State=#player_state{playerID=PlayerID}) ->
    Response = Controller:Action(PlayerID, Params),
    {reply, Response, State};
handle_call({load_data, ModelName}, _From, State=#player_state{playerID=PlayerID}) ->
    Result = load_data_from_db_to_ets(PlayerID, ModelName),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({clean_data, ModelName}, State=#player_state{playerID=PlayerID}) ->
    clean_data_from_ets(PlayerID, ModelName),
    {noreply, State};
handle_cast(circulation_persist_data, State=#player_state{playerID=PlayerID}) ->
    persist_player_all_models_data(PlayerID),
    erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#player_state{circulation_persist_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

player_pid(PlayerID) ->
    case ?GET_PID({player, PlayerID}) of
        undefined ->
            {ok, Pid} = player_factory:start_player(PlayerID),
            Pid;
        PlayerPid ->
            PlayerPid
    end.

con_pid(PlayerID) ->
    ?GET_PID({connection, PlayerID}).

send_data(PlayerID, Data) ->
    game_connection:send_data(con_pid(PlayerID), Data).

%% Persist one of player's models data to db.
persist_model_from_ets_to_db(PlayerID, ModelName) ->
    case player_data:get_player_record_status(PlayerID, ModelName) of
        [] ->
            ok;
        Changes ->
            lists:foreach(
                fun({Id, Status, Value}) ->
                    data_persister:persist(ModelName, Id, Status, Value),
                    player_data:del_record_status(PlayerID, ModelName, Id)
                end, Changes)
    end.

%% Persist all of the players data to db.
persist_player_all_models_data(PlayerID) ->
    case player_data:get_player_records_status(PlayerID) of
        [] ->
            ok;
        Changes ->
            lists:foreach(
                fun({ModelName, Id, Status, Value}) ->
                    data_persister:persist(ModelName, Id, Status, Value),
                    player_data:del_record_status(PlayerID, ModelName, Id)
                end, Changes)
    end.

clean_data_from_ets(PlayerID, ModelName) ->
    persist_model_from_ets_to_db(PlayerID, ModelName),
    player_data:clean(PlayerID, ModelName),
    ok.

load_data_from_db_to_ets(PlayerID, ModelName) ->
    case player_data:get_data_status({PlayerID, ModelName, loaded}) of
        undefined ->
            ModuleName = model_module_name(ModelName),
            {ok, Recs} = ModuleName:load_data(PlayerID),
            Tab = player_data:table(ModelName),
            if
                is_list(Recs) andalso Recs =/= [] ->
                    io:format("load_data: ~p Found Records~n", [ModelName]),
                    true = ets:insert(Tab, Recs);
                true ->
                    io:format("load_data: ~p Record Not Found~n", [ModelName]),
                    ok
            end,
            player_data:set_loaded(PlayerID, ModelName, true),
            Recs;
        _ ->
            false
    end.

model_module_name(ModelName) ->
    list_to_atom(atom_to_list(ModelName) ++ "_model").
