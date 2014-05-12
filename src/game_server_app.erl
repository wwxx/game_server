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


-module(game_server_app).
-behaviour(application).

-export([start/2]).
-export([prep_stop/1, stop/1]).

-include("include/db_config.hrl").

start(_Type, _Args) ->
    ensure_started(crypto),
    case application:get_env(game_server, server_environment) of
        {ok, test} -> ok;
        {ok, development} -> ok;
        _ -> ok = lager:start()
    end,
    mnesia:create_schema([node()]),
    mnesia:start(),
    game_counter:start(),
    ensure_started(gproc),
    DB_Config = case application:get_env(game_server, server_environment) of
        {ok, production} -> ?DB_PRODUCTION;
        {ok, development} -> ?DB_DEVELOPMENT;
        {ok, test} -> ?DB_TEST
    end,
    ensure_started(db),
    db:init_pool(DB_Config),
    ensure_started(record_mapper),
    ensure_started(game_numerical),
    ensure_started(player_server),
    ensure_started(leaderboard),
    ensure_started(timertask),

    life_cycle:before_start(),
    R = game_server_sup:start_link(),
    life_cycle:after_start(),
    R.

prep_stop(State) ->
    life_cycle:before_stop(),
    State.

stop(_State) ->
    life_cycle:after_stop(),
    ok.

-spec ensure_started(module()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
