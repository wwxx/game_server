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
%%%        Callbacks invoked by GameServer for initializing
%%% @end
%%% Created :  二  1 21 14:58:43 2014 by Savin-Max µ

-module(life_cycle).

-export([before_start/0,
		 after_start/0,
		 before_stop/0,
		 after_stop/0
		 ]).

%%%===================================================================
%%% Framework Callbacks
%%%===================================================================

%% before start game_server
before_start() ->
    error_logger:info_msg("Game Server Life Cycle Callback: before_start!~n"),
    %% add your custom initialize at here
    ok.

%% after start game_server
after_start() ->
    error_logger:info_msg("Game Server Life Cycle Callback: after_start!~n"),
    %% add your custom initialize at here
    model_mapping:load(),
    game_numerical:load_data(),
    after_game_server_start:setup(),
    ok.

before_stop() ->
    error_logger:info_msg("Game Server Life Cycle Callback: before_stop!~n"),
    %% shutdown tcp server
    supervisor:terminate_child(game_server_sup, {ranch_listener_sup, ranch_tcp_listener}),
    supervisor:terminate_child(game_server_sup, ranch_sup),
    %% shutdown auto executing events server
    %% ......
    %% Stop all player process and use terminate callback to persist data
    supervisor:terminate_child(player_base_sup, player_sup),
    %player_data:flush_to_mysql(),
    ok.

after_stop() ->
    error_logger:info_msg("Game Server Life Cycle Callback: after_stop!~n"),
    %% add your custom stopping at here
	ok.

%%%===================================================================
%%% Private Custom initialize functions
%%%===================================================================

