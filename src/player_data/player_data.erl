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
%%%       Manage player's ets data:
%%% @end
%%% Created :  日 10 06 23:34:35 2013 by Savin-Max µ

-module(player_data).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_player_id/1,
         create/2,
         delete/2,
         update/3,
         find/2,
         where/2,
         all/2,
         count/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE_TAB, player_data_state_tracker).
-define(ACTIVE_CHECK_DURATION, 600). % 6 minutes
-define(DATA_EXPIRE_DURATION, 1800). % 30 minutes

-include("include/db_schema.hrl").
-include("include/gproc_macros.hrl").

-record(state, {playerID, circulation_active_check_timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Only can be invoked by game_connection
get_player_id(Udid) ->
    users_model:get_player_id(Udid).

create(PlayerID, Record) when is_tuple(Record) ->
    track_active(PlayerID, Record),
    case validate_ownership(PlayerID) of
        true -> model:create(Record);
        false -> player:proxy(PlayerID, model, create, [Record])
    end;
create(PlayerID, Records) when is_list(Records) ->
    track_active(PlayerID, hd(Records)),
    case validate_ownership(PlayerID) of
        true ->
            lists:foreach(fun(Record) ->
                              model:create(Record)
                          end, Records);
        false ->
            lists:foreach(fun(Record) ->
                              player:proxy(PlayerID, model, create, [Record])
                          end, Records)
    end.

delete(PlayerID, Selector) ->
    track_active(PlayerID, Selector),
    case validate_ownership(PlayerID) of
        true -> model:delete(Selector);
        false -> player:proxy(PlayerID, model, delete, [Selector])
    end.

update(PlayerID, Selector, Modifier) ->
    track_active(PlayerID, Selector),
    case validate_ownership(PlayerID) of
        true -> model:update(Selector, Modifier);
        false -> player:proxy(PlayerID, model, update, [Selector, Modifier])
    end.

find(PlayerID, Selector) ->
    track_active(PlayerID, Selector),
    case validate_ownership(PlayerID) of
        true -> model:find(Selector);
        false -> player:proxy(PlayerID, model, find, [Selector])
    end.

where(PlayerID, Selector) ->
    track_active(PlayerID, Selector),
    case validate_ownership(PlayerID) of
        true -> model:where(Selector);
        false -> player:proxy(PlayerID, model, where, [Selector])
    end.

all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:all(Table);
        false -> player:proxy(PlayerID, model, all, [Table])
    end.

count(PlayerID, Selector) ->
    track_active(PlayerID, Selector),
    case validate_ownership(PlayerID) of
        true -> model:count(Selector);
        false -> player:proxy(PlayerID, model, count, [Selector])
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Timer = erlang:send_after(?ACTIVE_CHECK_DURATION, self(), circulation_active_check),
    {ok, #state{circulation_active_check_timer = Timer}}.

handle_call({set_data_status, Key, Value}, _From, State) ->
    Result = ets:insert(?STATE_TAB, {Key, Value}),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_active_check, State) ->
    check_active(),
    erlang:send_after(?ACTIVE_CHECK_DURATION, self(), circulation_active_check),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State=#state{circulation_active_check_timer=Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
validate_ownership(PlayerID) ->
    PlayerID =:= get(player_id).

track_active(PlayerID, Record) ->
    [ModelName|_] = tuple_to_list(Record),
    Key = {track_active, ModelName, PlayerID},
    Value = time_utils:current_time(),
    ets:insert(?STATE_TAB, {Key, Value}).

check_active() ->
    CurrentTime = time_utils:current_time(),
    lists:foreach(
        fun([ModelName, PlayerID, LastActive]) ->
            if
                (CurrentTime - LastActive) >= ?DATA_EXPIRE_DURATION ->
                    player:clean_data(PlayerID, ModelName);
                true ->
                    ok
            end
        end, active_specs()).

-spec(active_specs() -> [{ModelName::atom(), PlayerID::binary(), LastActive::integer()}]).
active_specs() ->
    ets:match(?STATE_TAB, {{track_active, '$1', '$2'}, '$3'}).
