-module (gms_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/db_schema.hrl").
-include("include/error_code.hrl").
-include("include/common_const.hrl").

-define(UDID, <<"eunit_self_udid">>).

copies_controller_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun add_hero/1,
      fun set_hero_level/1
    ]}.

start() ->
    game_server:start([test]),
    db:delete_all(heros),
    db:delete_all(users).

stop(_Pid) ->
    game_server:stop().

add_hero(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    AddHeroRes = fake_client:request(?UDID, gms_params, {[<<"add_hero">>, <<"2">>]}),
    Count = player_data:count(PlayerID, #heros{user_id = PlayerID}),
    [?_assertEqual(Count, 2)].

set_hero_level(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    Hero = player_data:find(PlayerID, #heros{config_id = 1}),
    LevelHeroRes = fake_client:request(?UDID, gms_params, {[<<"set_hero_level">>, <<"1">>, <<"10">>]}),
    error_logger:info_msg("set hero level res: ~p~n", [LevelHeroRes]),
    NewHero = player_data:find(PlayerID, #heros{uuid = Hero#heros.uuid}),
    [?_assertEqual(NewHero#heros.level, 10)].
