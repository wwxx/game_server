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

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Key, AfterSecs, MFA) ->
    gen_server:cast(?SERVER, {add, Key, AfterSecs, MFA}).

cancel(Key) ->
    gen_server:cast(?SERVER, {cancel, Key}).

update(Key, AfterSecs, MFA) ->
    gen_server:cast(?SERVER, {update, Key, AfterSecs, MFA}).

sync_add(Key, AfterSecs, MFA) ->
    gen_server:call(?SERVER, {add, Key, AfterSecs, MFA}).

sync_cancel(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

sync_update(Key, AfterSecs, MFA) ->
    gen_server:call(?SERVER, {update, Key, AfterSecs, MFA}).

lookup(Key) ->
    case ets:lookup(?TAB, Key) of
        [] -> undefined;
        [Result] -> Result
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({add, Key, AfterSecs, MFA}, _From, State) ->
    add_timer(Key, AfterSecs, MFA),
    {reply, ok, State};
handle_call({cancel, Key}, _From, State) ->
    cancel_timer(Key),
    {reply, ok, State};
handle_call({update, Key, AfterSecs, MFA}, _From, State) ->
    update_timer(Key, AfterSecs, MFA),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add, Key, AfterSecs, MFA}, State) ->
    add_timer(Key, AfterSecs, MFA),
    {noreply, State};
handle_cast({cancel, Key}, State) ->
    cancel_timer(Key),
    {noreply, State};
handle_cast({update, Key, AfterSecs, MFA}, State) ->
    update_timer(Key, AfterSecs, MFA),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timertask, Key, MFA}, State) ->
    ets:delete(?TAB, Key),
    case ets:lookup(?TAB, Key) of
        [] -> do_nothing;
        [{Key, Timer, _MFA}] -> erlang:cancel_timer(Timer)
    end,
    dispatch_to_worker(MFA),
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
add_timer(Key, AfterSecs, MFA) ->
    case ets:lookup(?TAB, Key) of
        [] ->
            Timer = erlang:send_after(AfterSecs, ?SERVER, {timertask, Key, MFA}),
            ets:insert_new(?TAB, {Key, Timer, MFA});
        _ -> do_nothing
    end.

cancel_timer(Key) ->
    ets:delete(?TAB, Key),
    case ets:lookup(?TAB, Key) of
        [] -> do_nothing;
        [{Key, Timer, _MFA}] ->
            erlang:cancel_timer(Timer)
    end.

update_timer(Key, AfterSecs, MFA) ->
    case ets:lookup(?TAB, Key) of
        [] -> do_nothing;
        [{Key, Timer, _MFA}] ->
            erlang:cancel_timer(Timer),
            NewTimer = erlang:send_after(AfterSecs, ?SERVER, {timertask, Key, MFA}),
            ets:insert_new(?TAB, {Key, NewTimer, MFA})
    end.


dispatch_to_worker(MFA) ->
    poolboy:transaction(timertask_worker_pool, fun(Worker) ->
        gen_server:call(Worker, {timertask, MFA})
    end).
