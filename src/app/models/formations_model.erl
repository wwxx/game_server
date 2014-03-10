-module (formations_model).
-include ("include/db_schema.hrl").
-export ([info/1, load_data/1]).

info(Formation) ->
	{Formation#formations.uuid,
	 Formation#formations.user_id,
	 Formation#formations.matrix}.

load_data(PlayerID) ->
	db:find_by(formations, user_id, PlayerID).