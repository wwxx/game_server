-module (heros_model).
-include ("include/db_schema.hrl").
-export ([info/1, infos/1, load_data/1]).

info(Hero) ->
    {Hero#heros.uuid, 
     Hero#heros.user_id,
     Hero#heros.level,
     Hero#heros.config_id}.

infos(Heros) ->
	lists:map(fun info/1, Heros).


load_data(PlayerID) ->
	db:find_by(heros, user_id, PlayerID).
