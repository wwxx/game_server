-module(record_mapper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    record_mapper = ets:new(record_mapper, [
                set, public, named_table, {read_concurrency, true}]),
    Server = {record_mapper, {record_mapper, start_link, [record_mapper]},
	      permanent, 2000, worker, [mongrel]},
	RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, [Server]}}.
