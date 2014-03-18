-module(copies_model).

-include ("include/db_schema.hrl").
-export ([info/1, infos/1, load_data/1]).

info(Copy) ->
    {Copy#copies.uuid,
     Copy#copies.area_id,
     Copy#copies.copy_id,
     Copy#copies.star,
     Copy#copies.battle_process,
     Copy#copies.is_rewarded}.

infos(Copies) ->
    lists:map(fun info/1, Copies).

load_data(PlayerID) ->
    db:find_by(copies, user_id, PlayerID).
