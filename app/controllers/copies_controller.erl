-module(copies_controller).

-export([infos/2]).
-include("include/db_schema.hrl").

infos(PlayerID, Params) ->
    AreaId = proplists:get_value(area_id, Params),
    case player_data:where(PlayerID, #copies{area_id = AreaId}) of
        [] ->
            {copies_info, []};
        Copies ->
            {copies_info, copies_model:infos(Copies)}
    end.
