-module(heros_controller).

-export([info/2, infos/2]).

-include("include/db_schema.hrl").
-include("app/include/error_code.hrl").


info(PlayerID, Params) ->
    HeroId = proplists:get_value(id, Params),
    case player_data:find(PlayerID, #heros{uuid = HeroId}) of
        undefined ->
            {fail, {?ERROR_HERO_NOT_FOUND, <<"">>}};
        Hero ->
            {hero, heros_model:info(Hero)}
    end.

infos(PlayerID, _Params) ->
    case  player_data:where(PlayerID, #heros{user_id = PlayerID}) of
      [] ->
          {heros_info, []};
      Heros ->
          {heros_info, {heros_model:infos(Heros)}}
    end.
