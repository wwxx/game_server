-module(formations_controller_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/db_schema.hrl").
-include("include/common_const.hrl").

-define(UDID, <<"eunit_self_udid">>).

formations_controller_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun info_tests/1,
      fun update_tests/1
     ]}.

start() ->
    game_server:start([test]),
    db:delete_all(users),
    db:delete_all(formations),
    db:delete_all(heros).

stop(_Pid) ->
    game_server:stop().

info_tests(_Pid) ->
    db:delete_all(users),
    PlayerID = player_data:get_player_id(?UDID),
    Res = fake_client:request(?UDID, formation_info_params, {PlayerID}),
    FormationList = proplists:get_value(matrix, Res),
    [?_assertEqual(proplists:get_value(user_id, Res), PlayerID),
     ?_assertEqual(length(FormationList), 9)
    ].

update_tests(_Pid) ->
    db:delete_all(users),
    PlayerID = player_data:get_player_id(?UDID),
    Matrix = [<<"empty">>, <<"empty">>, <<"empty">>,
              <<"empty">>, <<"empty">>, <<"empty">>,
              <<"empty">>, <<"empty">>, <<"empty">>],
    Res = fake_client:request(?UDID,
                              formation_update_params,
                              {Matrix}),
    Formation = player_data:find(PlayerID, #formations{user_id = PlayerID}),
    NewMatrix = binary_string:split(Formation#formations.matrix, <<",">>),
    [?_assertEqual(NewMatrix, Matrix),
     ?_assertEqual(Res, [{code, ?OK}])
    ].
