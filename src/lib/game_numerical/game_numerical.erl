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

all(Name) ->
    ets:match_object(table_name(Name), '$1').

find(Name, Key) ->
    Table = table_name(Name),
    case ets:lookup(Table, Key) of
        [Object] ->
            Object;
        [] ->
            undefined
    end.

find_element(Name, Key, Pos) ->
    Table = table_name(Name),
    ets:lookup_element(Table, Key, Pos).

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_config_data() ->
    {ok, FileNames} = file:list_dir(?DATA_DIR),
    lists:foreach(
        fun(FileName) ->
            BaseName = lists:nth(1, string:tokens(FileName, ".")),
            FileData = load_config(FileName),
            TableData = conver_to_table_object(FileData),
            Tab = ets:new(table_name(BaseName), [
                            set, named_table, protected, {read_concurrency, true}]),
            ets:insert(Tab, TableData)
        end, FileNames).


conver_to_table_object(Data) ->
    case is_list(Data) of
        true ->
            conver_to_table_object(Data, []);
        false ->
            Data
    end.

conver_to_table_object([], Result) ->
    lists:reverse(Result);
conver_to_table_object([Item|TailItems], Result) ->
    case is_list(Item) of
        true ->
            conver_to_table_object(TailItems, [list_to_tuple(Item)|Result]);
        false ->
            conver_to_table_object(TailItems, [Item|Result])
    end.

load_config(FileName) ->
    FilePath = file_path(FileName),
    [Result] = yamerl_constr:file(FilePath),
    Result.

file_path(FileName) ->
    ?DATA_DIR ++ FileName.

table_name(Name) ->
    list_to_atom("config_data_" ++ Name).
