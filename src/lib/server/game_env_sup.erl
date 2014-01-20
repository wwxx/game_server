-module (game_env_sup).

-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                    permanent, 5000, Type, [Mod]}).

%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
    game_env = ets:new(game_env, [ordered_set, public, named_table, {read_concurrency, true}]),
    Env = ?CHILD(game_env, game_env, worker, []),
    {ok, {{one_for_one, 5, 10}, [Env]}}.
