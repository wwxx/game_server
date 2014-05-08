%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2024
%%% Savin Max <mafei.198@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
%%% @doc
%%%        Manage timertask!
%%% @end
%%% Created :  一  3 24 18:20:18 2014 by Me

-module(timertask).

-behaviour(gen_server).

%% API
-export([start_link/0, 
         sync_add/3, sync_cancel/1, sync_update/3,
         add/3, cancel/1, update/3, lookup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

-define(TABLE, game_server_timertask).
-define(ORDER_TABLE, game_server_ordered_timertask).

-record(?TABLE, {key, run_at, mfa}).
-record(?ORDER_TABLE, {key, mfa}).


%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Key, RunAt, MFA) ->
    gen_server:cast(?SERVER, {add, Key, RunAt, MFA}).

cancel(Key) ->
    gen_server:cast(?SERVER, {cancel, Key}).

update(Key, RunAt, MFA) ->
    gen_server:cast(?SERVER, {update, Key, RunAt, MFA}).

sync_add(Key, RunAt, MFA) ->
    gen_server:call(?SERVER, {add, Key, RunAt, MFA}).

sync_cancel(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

sync_update(Key, RunAt, MFA) ->
    gen_server:call(?SERVER, {update, Key, RunAt, MFA}).

lookup(Key) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [] -> undefined;
        [{?TABLE, Key, RunAt, MFA}] -> {Key, RunAt, MFA}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    mnesia:create_table(?TABLE, [{disc_copies, [node()]},
                                 {type, set},
                                 {attributes, record_info(fields, ?TABLE)}]),
    mnesia:create_table(?ORDER_TABLE, [{disc_copies, [node()]},
                                       {type, ordered_set},
                                       {attributes, record_info(fields, ?ORDER_TABLE)}]),
    {ok, #state{}}.

handle_call({add, Key, RunAt, MFA}, _From, State) ->
    add_timer(Key, RunAt, MFA),
    {reply, ok, State};
handle_call({cancel, Key}, _From, State) ->
    cancel_timer(Key),
    {reply, ok, State};
handle_call({update, Key, RunAt, MFA}, _From, State) ->
    update_timer(Key, RunAt, MFA),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add, Key, RunAt, MFA}, State) ->
    add_timer(Key, RunAt, MFA),
    {noreply, State};
handle_cast({cancel, Key}, State) ->
    cancel_timer(Key),
    {noreply, State};
handle_cast({update, Key, RunAt, MFA}, State) ->
    update_timer(Key, RunAt, MFA),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timertask, Key, MFA}, State) ->
    dispatch_to_worker(MFA),
    cancel_timer(Key),
    {noreply, State};
handle_info(Info, State) ->
    error_logger:info_msg("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_timer(Key, RunAt, MFA) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [] ->
            mnesia:dirty_write({?TABLE, Key, RunAt, MFA}),
            mnesia:dirty_write({?ORDER_TABLE, {RunAt, Key}, MFA}),
            try_restart_timer();
        _ -> ok
    end.

cancel_timer(Key) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [{?TABLE, Key, RunAt, _MFA}] ->
            mnesia:dirty_delete(?TABLE, Key),
            mnesia:dirty_delete(?ORDER_TABLE, {RunAt, Key});
        _ -> ok
    end,
    case get(current_timer) of
        {Key, _RunAt, Timer} ->
            erlang:cancel_timer(Timer),
            erase(current_timer),
            try_restart_timer();
        undefined -> ok
    end.

update_timer(Key, RunAt, MFA) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [{?TABLE, Key, OldRunAt, _OldMFA}] ->
            mnesia:dirty_write({?TABLE, Key, RunAt, MFA}),
            mnesia:dirty_delete(?ORDER_TABLE, {OldRunAt, Key}),
            mnesia:dirty_write({?ORDER_TABLE, {RunAt, Key}, MFA});
        _ -> ok
    end,
    case get(current_timer) of
        {_Key, CRunAt, _MFA} ->
            case CRunAt > RunAt of
                true -> try_restart_timer();
                false -> ok
            end;
        undefined -> try_restart_timer()
    end.

dispatch_to_worker(MFA) ->
    poolboy:transaction(timertask_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {timertask, MFA})
    end).

check_first_timer_changed() ->
    case mnesia:dirty_first(?ORDER_TABLE) of
        '$end_of_table' -> false;
        {RunAt, Key} ->
            case get(current_timer) of
                {Key, _RunAt, _Timer} -> 
                    false;
                undefined -> 
                    {Key, RunAt, true};
                {_OtherKey, Timer} -> 
                    erlang:cancel_timer(Timer),
                    {Key, RunAt, true}
            end
    end.

try_restart_timer() ->
    case check_first_timer_changed() of
        false -> ok;
        {Key, RunAt, true} ->
            case mnesia:dirty_read(?TABLE, Key) of
                [] -> 
                    mnesia:dirty_delete(?ORDER_TABLE, {RunAt, Key});
                [{?TABLE, Key, RunAt, MFA}] ->
                    AfterTime = lists:max([(RunAt - time_utils:now()) * 1000, 0]),
                    Timer = erlang:send_after(AfterTime, ?SERVER, {timertask, Key, MFA}),
                    put(current_timer, {Key, RunAt, Timer})
            end
    end.
