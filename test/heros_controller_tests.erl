-module(heros_controller_tests).
-include_lib("eunit/include/eunit.hrl").
-include ("include/db_schema.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define (UDID, <<"eunit_self_udid">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
heros_controller_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun info_tests/1
      %fun infos_tests/1
     ]}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    game_server:start().

stop(_Pid) ->
    game_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
info_tests(_Pid) ->
    Udid = <<"eunit_test_udid">>,
    PlayerID = player_data:get_player_id(Udid),

    R = player_data:find(PlayerID, #users{uuid = PlayerID}),

    Hero = player_data:find(PlayerID, #heros{user_id = PlayerID}),
    Res = fake_client:request(Udid, hero_info_params, {Hero#heros.uuid}),
    io:format("Res: ~p~n", [Res]),

    [?_assert(erlang:is_binary(PlayerID)),
     ?_assertEqual(R#users.udid, Udid),
     ?_assertNotEqual(PlayerID, <<"">>)
    ].

infos_tests(_Pid) ->
    Udid = <<"eunit_test_udid">>,
    PlayerID = player_data:get_player_id(Udid),

    R = player_data:find(PlayerID, #users{uuid = PlayerID}),

    Res = fake_client:request(Udid, heros_info_params, {PlayerID}),
    io:format("Res: ~p~n", [Res]),

    [?_assert(erlang:is_binary(PlayerID)),
     ?_assertEqual(R#users.udid, Udid),
     ?_assertNotEqual(PlayerID, <<"">>)
    ].


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
