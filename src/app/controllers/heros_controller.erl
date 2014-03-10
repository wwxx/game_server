-module(heros_controller).

-export([info/2, infos/2]).

-include("include/db_schema.hrl").
-include("include/error_code.hrl").


info(PlayerID, Params) ->
    HeroId = proplists:get_value(id, Params),
    io:format("requested hero info~n"),
    case player_data:find(PlayerID, HeroId) of
        undefined ->
            api_encoder:encode(error, {?ERROR_HERO_NOT_FOUND, <<"">>});
        Hero ->
            api_encoder:encode(hero, heros_model:info(Hero))
    end.

infos(PlayerID, _Params) ->
    io:format("requested heros info~n"),
    io:format("PlayerID: ~p~n", [PlayerID]),
    case  player_data:where(PlayerID, #heros{user_id = PlayerID}) of
      [] ->
          api_encoder:encode(heros_info, {[]});
      Heros ->
          api_encoder:encode(heros_info, {heros_model:infos(Heros)})
    end.
