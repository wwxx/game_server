%%%-------------------------------------------------------------------
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) 2013 Savin-Max. All Rights Reserved.
%%% @doc
%%%        Load Game Config data from YAML files to ets table for later using.
%%% @end
%%% Created :  一 10 07 23:50:49 2013 by Savin-Max
%%%-------------------------------------------------------------------
-module(game_numerical).

-behaviour(gen_server).

%% API
-export([start_link/0, find/2, find_element/3, all/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DATA_DIR, "config/game_data/").

-record(state, {table}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

all(TableName) ->
    ets:match_object(TableName, '$1').

find(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [Object] ->
            Object;
        [] ->
            undefined
    end.

find_element(TableName, Key, Pos) ->
    ets:lookup_element(TableName, Key, Pos).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    load_config_data(),
    {ok, #state{}}.

handle_call(load_data, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Info:~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_config_data() ->
    Cmd = "ruby lib/import_config.rb",
    Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio, exit_status, binary]),
    Payload = term_to_binary({import_config, <<"">>}),
    port_command(Port, Payload),
    receive_config_data(Port).

receive_config_data(Port) ->
    receive
        {Port, {data, Data}} ->
            case binary_to_term(Data) of
                {create_table, TableName} ->
                    io:format("TableName:~p~n~n", [TableName]),
                    ets:new(TableName,
                            [set, named_table, protected, {read_concurrency, true}]),
                    receive_config_data(Port);
                {insert, TableName, Values} ->
                    io:format("Values:~p~n~n", [Values]),
                    ets:insert(TableName, Values),
                    receive_config_data(Port);
                finish ->
                    ok
            end
    end.
