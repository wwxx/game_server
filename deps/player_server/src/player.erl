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
         stop/1,
         request/4,
         send_data/2,
         save_data/1,
         sync_save_data/1,
         player_pid/1,
         proxy/4,
         async_proxy/4,
         wrap/2,
         async_wrap/2,
         subscribe/2,
         unsubscribe/2,
         publish/3,
         on_tcp_closed/1,
         register_tcp_closed_callback/4,
         cancel_tcp_closed_callback/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(player_state, {playerID,
                       circulation_persist_timer}).

-define(PERSIST_DURATION, 1800000). %% 30 minutes
-define(EXPIRE_DURATION,  1800). %% 30 minutes

-include("player_macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(PlayerID) ->
    gen_server:start_link(?MODULE, [PlayerID], []).

stop(PlayerID) ->
    case ?GET_PID({player, PlayerID}) of
        undefined -> ok;
        PlayerPID -> gen_server:cast(PlayerPID, {stop, shutdown})
    end.

request(PlayerID, Path, Params, RequestId) ->
    gen_server:cast(player_pid(PlayerID), {request, Path, Params, RequestId}).

% send_data(PlayerID, Data) ->
%     case con_pid(PlayerID) of
%         undefined -> do_nothing;
%         ConPid -> game_connection:send_data(ConPid, Data)
%     end.

send_data(PlayerID, Data) ->
    case validate_ownership(PlayerID) of
        true -> 
            case is_requesting() of
                true -> 
                    pending_response(Data);
                false ->
                    send_data_to_connection(PlayerID, Data)
            end;
        false ->
            send_data_to_connection(PlayerID, Data)
    end.

send_data_to_connection(PlayerID, Data) ->
    case con_pid(PlayerID) of
        undefined -> do_nothing;
        ConPid -> game_connection:send_data(ConPid, Data)
    end.

save_data(PlayerID) ->
    gen_server:cast(player_pid(PlayerID), {save_data}).

sync_save_data(PlayerID) ->
    gen_server:call(player_pid(PlayerID), {save_data}).

proxy(PlayerID, Module, Fun, Args) ->
    case validate_ownership(PlayerID) of
        true ->
            track_active(),
            apply(Module, Fun, Args);
        false ->
            gen_server:call(player_pid(PlayerID), {proxy, Module, Fun, Args})
    end.

async_proxy(PlayerID, Module, Fun, Args) ->
    gen_server:cast(player_pid(PlayerID), {proxy, Module, Fun, Args}).

wrap(PlayerID, Fun) ->
    case validate_ownership(PlayerID) of
        true -> 
            track_active(),
            Fun();
        false ->
            gen_server:call(player_pid(PlayerID), {wrap, Fun})
    end.

async_wrap(PlayerID, Fun) ->
    gen_server:cast(player_pid(PlayerID), {wrap, Fun}).

subscribe(PlayerID, Channel) ->
    gen_server:cast(player_pid(PlayerID), {subscribe, Channel}).
unsubscribe(PlayerID, Channel) ->
    gen_server:cast(player_pid(PlayerID), {unsubscribe, Channel}).
publish(PlayerID, Channel, Msg) ->
    gen_server:cast(player_pid(PlayerID), {publish, Channel, Msg}).

on_tcp_closed(PlayerID) ->
    gen_server:cast(player_pid(PlayerID), {on_tcp_closed}).

register_tcp_closed_callback(PlayerID, Key, Type, Fun) ->
    gen_server:cast(player_pid(PlayerID), {register_tcp_closed_callback, Key, Type, Fun}).

cancel_tcp_closed_callback(PlayerID, Key) ->
    gen_server:cast(player_pid(PlayerID), {cancel_tcp_closed_callback, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerID]) ->
    ?REG_PID({player, PlayerID}),
    put(player_id, PlayerID),
    Timer = erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
    process_flag(trap_exit, true),
    {ok, #player_state{playerID=PlayerID, circulation_persist_timer=Timer}}.

handle_call({proxy, Module, Fun, Args}, _From, State) ->
    track_active(),
    Result = erlang:apply(Module, Fun, Args),
    {reply, Result, State};
handle_call({wrap, Fun}, _From, State) ->
    track_active(),
    {reply, Fun(), State};
handle_call({save_data}, _From, State) ->
    model:persist_all(),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({proxy, Module, Fun, Args}, State) ->
    track_active(),
    erlang:apply(Module, Fun, Args),
    {noreply, State};
handle_cast({wrap, Fun}, State) ->
    track_active(),
    Fun(),
    {noreply, State};
handle_cast({request, {Controller, Action}, Params, RequestId},
            State=#player_state{playerID=PlayerID}) ->
    track_active(),
    begin_request(),
    try Controller:Action(PlayerID, Params) of
        Response -> 
            CachedData = [{RequestId ,Response}|get_pending_responses()],
            del_pending_responses(),
            send_multi_data(PlayerID, lists:reverse(CachedData))
    catch
        Type:Msg ->
            exception:notify(Type, Msg, Controller, Action, 
                             [{playerID, PlayerID}|Params])
    end,
    finish_request(),
    {noreply, State};
handle_cast({stop, shutdown}, State) ->
    {stop, shutdown, State};
handle_cast({save_data}, State) ->
    model:persist_all(),
    {noreply, State};
handle_cast({subscribe, Channel}, State) ->
    List = proplists:get_value(gproc, gproc:info(self())),
    case proplists:is_defined({p, l, Channel}, List) of
        true -> ok;
        false -> ?SUBSCRIBE(Channel)
    end,
    {noreply, State};
handle_cast({unsubscribe, Channel}, State) ->
    case proplists:get_value(gproc, gproc:info(self())) of
        [] -> ?UNSUBSCRIBE(Channel);
        _ -> ok
    end,
    List = proplists:get_value(gproc, gproc:info(self())),
    case proplists:is_defined({p, l, Channel}, List) of
        true -> ?UNSUBSCRIBE(Channel);
        false -> ok
    end,
    {noreply, State};
handle_cast({publish, Channel, Msg}, State) ->
    ?PUBLISH(Channel, {gproc_msg, Channel, Msg}),
    {noreply, State};
handle_cast({on_tcp_closed}, State) ->
    case get(tcp_closed_callback) of
        undefined -> ok;
        Callbacks ->
            NewCallbacks = lists:foldl(fun({Key, Type, Fun}, Result) ->
                try Fun() of
                    _Response -> ok
                catch
                    Exception:Msg ->
                        exception:notify(Exception, Msg)
                end,
                case Type of
                    callback_once -> Result;
                    callback_ever -> [{Key, Type, Fun}|Result]
                end
            end, [], Callbacks),
            put(tcp_closed_callback, NewCallbacks)
    end,
    {noreply, State};
handle_cast({register_tcp_closed_callback, Key, Type, Fun}, State) ->
    case get(tcp_closed_callback) of
        undefined -> put(tcp_closed_callback, [{Key, Type, Fun}]);
        List -> put(tcp_closed_callback, [{Key, Type, Fun}|List])
    end,
    {noreply, State};
handle_cast({cancel_tcp_closed_callback, DelKey}, State) ->
    case get(tcp_closed_callback) of
        undefined -> ok;
        Callbacks ->
            NewCallbacks = lists:foldl(fun({Key, Type, Fun}, Result) ->
                case Key =:= DelKey of
                    true -> Result;
                    false -> [{Key, Type, Fun}|Result]
                end
            end, [], Callbacks),
            put(tcp_closed_callback, NewCallbacks)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(circulation_persist_data, State=#player_state{circulation_persist_timer=Timer}) ->
    case time_utils:current_time() - get_last_active() >= ?EXPIRE_DURATION of
        true ->
            model:persist_all(),
            {stop, {shutdown, data_persisted}, State};
        false ->
            model:persist_all(),
            erlang:cancel_timer(Timer),
            NewTimer = erlang:send_after(?PERSIST_DURATION, self(), circulation_persist_data),
            {noreply, State#player_state{circulation_persist_timer=NewTimer}}
    end;
handle_info({gproc_msg, Channel, Msg}, State=#player_state{playerID=PlayerID}) ->
    player_subscribe:handle(Channel, PlayerID, Msg),
    {noreply, State};
handle_info({'EXIT', _, Reason}, State) ->
    io:format("RECEIVED EXIT SINGAL! Reason:~p~n", [Reason]),
    {stop, shutdown, State};
handle_info({shutdown, From}, State) ->
    model:persist_all(),
    From ! {finished_shutdown, self()},
    {stop, {shutdown, data_persisted}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State=#player_state{circulation_persist_timer=Timer}) ->
    case Reason of
        {shutdown, data_persisted} -> ok;
        _ -> model:persist_all()
    end,
    erlang:cancel_timer(Timer),
    gproc:goodbye(),
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
        PlayerPid -> PlayerPid
    end.

con_pid(PlayerID) ->
    ?GET_PID({connection, PlayerID}).

track_active() ->
    put({player, last_active}, time_utils:current_time()).

get_last_active() ->
    case get({player, last_active}) of
        undefined -> 0;
        LastActive -> LastActive
    end.

validate_ownership(PlayerID) ->
    PlayerID =:= get(player_id).

send_multi_data(PlayerID, MultiData) ->
    case con_pid(PlayerID) of
        undefined -> do_nothing;
        ConPid -> game_connection:send_multi_data(ConPid, MultiData)
    end.

pending_response(Data) ->
    case get(pending_responses) of
        undefined -> 
            put(pending_responses, [{0, Data}]);
        PendingResponses ->
            put(pending_responses, [{0, Data}|PendingResponses])
    end.

get_pending_responses() ->
    case get(pending_responses) of
        undefined -> [];
        CachedResponses -> CachedResponses
    end.

del_pending_responses() ->
    erase(pending_responses).

is_requesting() ->
    case get({status, is_requesting}) of
        undefined -> false;
        true -> true
    end.

begin_request() ->
    put({status, is_requesting}, true).

finish_request() ->
    erase({status, is_requesting}).
