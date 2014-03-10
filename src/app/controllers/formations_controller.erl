-module (formations_controller).
-include ("include/db_schema.hrl").
-include ("include/error_code.hrl").
-export ([info/2, update/2]).

info(PlayerID, Params) ->
	case player_data:find(PlayerID, #formations{user_id = PlayerID}) of
		undefined ->
			api_encoder:encode(error, {?FORMATION_NOT_FOUND, <<"">>});
		Formation ->
			api_encoder:encode(formation, formations_model:info(Formation))
	end.

update(PlayerID, Params) ->
	FormationId = proplists:get_value(id, Params),
	Matrix = proplists:get_value(matrix, Params),
	SelectorRecord = #formations{uuid = FormationId},
	ModifierRecord = #formations{matrix = Matrix},
	player_data:update(PlayerID, SelectorRecord, ModifierRecord),
	api_encoder:encode(success, {?OK}).
