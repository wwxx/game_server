-module(copies_controller_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/db_schema.hrl").
-include("src/app/include/error_code.hrl").
-include("include/common_const.hrl").

-define(UDID, <<"eunit_self_udid">>).

copies_controller_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun infos_tests/1]}.

start() ->
    game_server:start([test]),
    db:delete_all(users),
    db:delete_all(formations),
    db:delete_all(heros),
    db:delete_all(copies).

stop(_Pid) ->
    game_server:stop().

infos_tests(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    AreaId = 1,
    TmpRes = fake_client:request(?UDID, copies_info_params, {AreaId}),
    Copy = #copies{
              uuid = uuid_factory:gen(),
              user_id = PlayerID,
              area_id = 1,
              copy_id = 1,
              star = 1,
              battle_process = 2,
              is_rewarded = ?TRUE
             },
    player:proxy(PlayerID, player_data, create, [PlayerID, Copy]),
    Res = fake_client:request(?UDID, copies_info_params, {AreaId}),
    error_logger:info_msg("copies_controller_response: ~p~n", [Res]),
    [?_assertEqual(length(proplists:get_value(copies, TmpRes)), 0),
     ?_assertEqual(length(proplists:get_value(copies, Res)), 1)
    ].
