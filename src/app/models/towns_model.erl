-module(towns_model).

-export ([load_data/1]).

-include ("include/db_schema.hrl").

load_data(PlayerID) ->
    db:where(#towns{user_id = PlayerID}).
