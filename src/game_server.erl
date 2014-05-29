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


-module(game_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0, start/1, stop/0]).

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

start() ->
    start([development]).

start([Mode]) ->
    if
        Mode =:= production -> ok;
        Mode =:= test -> ok;
        Mode =:= development -> ensure_started(sync)
    end,
    error_logger:info_msg("Game Server Starting~n"),
    application:set_env(game_server, server_environment, Mode),
    ok = application:start(game_server).

stop() ->
    case application:get_env(game_server, server_environment) of
        {ok, test} -> force_stop();
        _ -> gen_server:cast(?SERVER, stop)
    end.

force_stop() ->
    application:stop(timertask),
    application:stop(leaderboard),
    application:stop(player_server),
    application:stop(game_server),
    application:stop(game_numerical),
    application:stop(record_mapper),
    application:stop(db),
    application:stop(emysql),
    application:stop(gproc),
    application:stop(crypto),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    error_logger:info_msg("==========PREPARING SHUTDOWN APPLICATION===========~n"),
    player_factory:shutdown_players(),
    error_logger:info_msg("================SHUTDOWNING PLAYERS================~n"),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({finished_shutdown_players, _From}, State) ->
    error_logger:info_msg("==========FINISHED TO SHUTDOWN ALL THE PLAYERS===========~n"),
    init:stop(),
    error_logger:info_msg("======INVOKE init:stop() TO STOP WHOLE APPLICATION=======~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
    {ok, Hostname} = inet:gethostname(),
    RemoteNode = list_to_atom("console@" ++ Hostname),
    {stop_deamon, RemoteNode} ! stopped,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec ensure_started(module()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
