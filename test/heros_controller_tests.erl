-module(heros_controller_tests).
-include_lib("eunit/include/eunit.hrl").
-include ("include/db_schema.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define (UDID, <<"eunit_self_udid">>). 

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
heros_controller_test_() ->
    {"Heros Controller tests.",
     ?setup(fun tests/1)}.

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
tests(_Pid) ->
    Udid = <<"eunit_test_udid">>,
    % db:delete_by(users, udid, Udid),
    PlayerID = player_data:get_player_id(Udid),

    R = player_data:find(PlayerID, #users{uuid = PlayerID}),

    Res = fake_client:request(Udid, heros_info_params, {PlayerID}),
    io:format("Res: ~p~n", [Res]),

    [?_assert(erlang:is_binary(PlayerID)),
     ?_assertEqual(R#users.udid, Udid),
     ?_assertNotEqual(PlayerID, <<"1">>)
    ].


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
