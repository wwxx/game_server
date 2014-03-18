-module (formations_controller).
-include ("include/db_schema.hrl").
-include ("include/common_const.hrl").
-include ("app/include/error_code.hrl").
-export ([info/2, update/2]).

info(PlayerID, _Params) ->
    case player_data:find(PlayerID, #formations{user_id = PlayerID}) of
        undefined ->
            {fail, {?FORMATION_NOT_FOUND, <<"">>}};
        Formation ->
            {formation, formations_model:info(Formation)}
    end.

update(PlayerID, Params) ->
    Matrix = proplists:get_value(matrix, Params),
    SelectorRecord = #formations{user_id = PlayerID},
    ModifierRecord = #formations{matrix = binary_string:join(Matrix, <<",">>)},
    player_data:update(PlayerID, SelectorRecord, ModifierRecord),
    {ok, {?OK}}.
