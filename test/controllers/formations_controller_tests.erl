-module(formations_controller_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/db_schema.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(UDID, <<"eunit_self_udid">>).

formations_controller_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun info_tests/1,
      fun update_tests/1
     ]}.

start() ->
    game_server:start().

stop(_Pid) ->
    game_server:stop().

info_tests(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    Res = fake_client:request(?UDID, formation_info_params, {PlayerID}),
    [?_assertEqual(Res, [])].

update_tests(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    Matrix = [<<"empty">>, <<"empty">>, <<"empty">>,
              <<"empty">>, <<"empty">>, <<"empty">>,
              <<"empty">>, <<"empty">>, <<"empty">>],
    Res = fake_client:request(?UDID,
                              formation_update_params,
                              {PlayerID, Matrix}),
    [?_assertEqual(Res, Matrix)].
