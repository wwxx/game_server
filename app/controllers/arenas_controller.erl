-module(arenas_controller).
-export([fight/2]).
-include ("include/db_schema.hrl").

fight(PlayerID, _Params) ->
    Players = player_data:where(PlayerID, #users{}),
    Infos = users_model:infos(Players),
    {users_info, Infos}.
