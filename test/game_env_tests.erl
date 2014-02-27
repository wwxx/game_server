-module(game_env_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun curd/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    {ok, Pid} = game_env_sup:start_link(),
    Pid.

stop(Pid) ->
    supervisor:terminate_child(Pid, whereis(game_env)).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
curd(_Pid) ->
    [?_assert(erlang:is_process_alive(whereis(game_env))),
     ?_assertEqual(game_env:get(name), undefined),
     ?_assert(game_env:set(name, savin)),
     ?_assertEqual(game_env:exists(name), true),
     ?_assertEqual(game_env:get(name), savin),
     ?_assert(game_env:del(name)),
     ?_assertEqual(game_env:get(name), undefined)
    ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
