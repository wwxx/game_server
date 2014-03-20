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


-module(player).

-behaviour(gen_server).

%% API
-export([start_link/1,
         request/3,
         send_data/2,
         clean_data/2,
         save_data/1,
         player_pid/1,
         proxy/4,
         subscribe/2,
         unsubscribe/2,
         publish/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(player_state, {playerID, circulation_persist_timer}).

% -define(PERSIST_DURATION, 1800000). %% 30 minutes
-define(PERSIST_DURATION, 200000). %% 30 minutes

-include("include/gproc_macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(PlayerID) ->
    gen_server:start_link(?MODULE, [PlayerID], []).

request(PlayerID, Path, Params) ->
    gen_server:call(player_pid(PlayerID), {request, Path, Params}).

clean_data(PlayerID, ModelName) ->
    gen_server:cast(player_pid(PlayerID), {clean_data, ModelName}).

save_data(PlayerID) ->
    gen_server:cast(player_pid(PlayerID), {save_data}).

proxy(PlayerID, Module, Fun, Args) ->
    gen_server:call(player_pid(PlayerID), {proxy, Module, Fun, Args}).

subscribe(PlayerID, Channel) ->
    gen_server:cast(player_pid(PlayerID), {subscribe, Channel}).
unsubscribe(PlayerID, Channel) ->
    gen_server:cast(player_pid(PlayerID), {unsubscribe, Channel}).
publish(PlayerID, Channel, Msg) ->
    gen_server:cast(player_pid(PlayerID), {publish, Channel, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerID]) ->
    io:format("Player: ~p started with Pid: ~p~n", [PlayerID, self()]),
    ?REG_PID({player, PlayerID}),
    put(player_id, PlayerID),
    Timer = erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    {ok, #player_state{playerID=PlayerID, circulation_persist_timer = Timer}}.

handle_call({request, {Controller, Action}, Params}, _From,
            State=#player_state{playerID=PlayerID}) ->
    Response = Controller:Action(PlayerID, Params),
    {reply, Response, State};
handle_call({proxy, Module, Fun, Args}, _From, State) ->
    Result = erlang:apply(Module, Fun, Args),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({clean_data, ModelName}, State=#player_state{playerID=PlayerID}) ->
    clean_data_from_memory(PlayerID, ModelName),
    {noreply, State};
handle_cast({save_data}, State=#player_state{playerID=PlayerID}) ->
    model:persist_all(),
    io:format("Manually saved player's data (PlayerID: ~p)~n", [PlayerID]),
    {noreply, State};
handle_cast({subscribe, Channel}, State) ->
    ?SUBSCRIBE(Channel),
    {noreply, State};
handle_cast({unsubscribe, Channel}, State) ->
    ?UNSUBSCRIBE(Channel),
    {noreply, State};
handle_cast({publish, Channel, Msg}, State) ->
    ?PUBLISH(Channel, {gproc_msg, Channel, Msg}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_persist_data, State) ->
    model:persist_all(),
    erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    {noreply, State};
handle_info({gproc_msg, Channel, Msg}, State=#player_state{playerID=PlayerID}) ->
    player_subscribe:handle(Channel, PlayerID, Msg),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#player_state{circulation_persist_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    gproc:goodbye(),
    model:persist_all(),
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

clean_data_from_memory(PlayerID, ModelName) ->
    model:persist_table(ModelName),
    player_data:clean(PlayerID, ModelName),
    ok.
