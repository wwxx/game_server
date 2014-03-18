-module(arenas_controller_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/db_schema.hrl").
-include("src/app/include/error_code.hrl").
-include("include/common_const.hrl").

-define(UDID, <<"eunit_self_udid">>).

copies_controller_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun fight_tests/1]}.

start() ->
    game_server:start([test]),
    db:delete_all(users),
    db:delete_all(formations),
    db:delete_all(heros),
    db:delete_all(copies).

stop(_Pid) ->
    game_server:stop().

fight_tests(_Pid) ->
    _PlayerID = player_data:get_player_id(?UDID),
    Res = fake_client:request(?UDID, arenas_fight_params, {}),
    error_logger:info_msg("arenas_controller_response: ~p~n", [Res]),
    [?_assertEqual(length(proplists:get_value(users, Res)), 1)
    ].
