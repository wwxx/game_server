%% Feel free to use, reuse and abuse the code in this file.

%% @private
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
