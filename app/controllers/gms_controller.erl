-module (gms_controller).
-export ([cmd/2]).
-include ("include/db_schema.hrl").
-include ("include/common_const.hrl").

cmd(PlayerID, Params) ->
    case proplists:get_value(cmd, Params) of
        [<<"add_hero">>, ConfigIdStr] ->
            {ConfigId, _} = string:to_integer(erlang:binary_to_list(ConfigIdStr)),
            add_hero(PlayerID, ConfigId),
            Hero = player_data:find(PlayerID, #heros{config_id = ConfigId}),
            {hero, heros_model:info(Hero)};
        [<<"set_hero_level">>, ConfigIdStr, LevelStr] ->
            {ConfigId, _} = string:to_integer(erlang:binary_to_list(ConfigIdStr)),
            {Level, _} = string:to_integer(erlang:binary_to_list(LevelStr)),
            error_logger:info_msg("ConfigId: ~p, Level: ~p~n", [ConfigId, Level]),
            player_data:update(PlayerID, #heros{config_id=ConfigId}, #heros{level=Level}),
            Hero = player_data:find(PlayerID, #heros{config_id = ConfigId}),
            {hero, heros_model:info(Hero)};
        true ->
            {fail, {?ERROR, <<"This Cmd not support!">>}}
    end.
    

add_hero(PlayerID, ConfigID) ->
    Hero = #heros{user_id = PlayerID,
                  config_id = ConfigID,
                  level = 1,
                  created_at = time_utils:datetime(),
                  updated_at = time_utils:datetime()
                 },
    player_data:create(PlayerID, Hero).