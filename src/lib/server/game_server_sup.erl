%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2014
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


-module(game_server_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    io:format("pusher_sup start_link"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    EnvSupSpec = ?CHILD(game_env_sup, game_env_sup, supervisor, []),
    RanchSupSpec = ?CHILD(ranch_sup, ranch_sup, supervisor, []),
    ListenerSpec = ranch:child_spec(ranch_tcp_listener, 1,
        ranch_tcp, [{port, 5555}],
        game_connection, []
    ),
    RecordMapperSupSpec = ?CHILD(record_mapper_sup, record_mapper_sup, supervisor, []),
    DBSupSpec = ?CHILD(db_sup, db_sup, supervisor, []),
    PlayerDataSupSpec = ?CHILD(player_data_sup, player_data_sup, supervisor, []),
    PlayerBaseSupSpec = ?CHILD(player_base_sup, player_base_sup, supervisor, []),
    GameNumericalSupSpec = ?CHILD(game_numerical_sup, game_numerical_sup, worker, []),
    Specs = [EnvSupSpec, RanchSupSpec, ListenerSpec,
             RecordMapperSupSpec, DBSupSpec, PlayerDataSupSpec,
             PlayerBaseSupSpec, GameNumericalSupSpec],
    {ok, {{one_for_one, 10, 10}, Specs}}.
