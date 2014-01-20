-module(player_data_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

-define(TAB, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(PlayerID) ->
    player_data_manager:start_child(PlayerID).

stop_child(PlayerID) ->
    player_data_manager:stop_child(PlayerID).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    PlayerDataHolderSpec = ?CHILD(player_data_holder, player_data_holder, worker, []),
    PlayerDataSpec = ?CHILD(player_data, player_data, worker, []),
    {ok, {{one_for_one, 5, 10}, [PlayerDataHolderSpec, PlayerDataSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
