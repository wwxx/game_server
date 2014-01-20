-module(items_model).

-export([load_data/1]).

-include("include/db_schema.hrl").

load_data(PlayerID) ->
    db:where(#items{user_id = PlayerID}).
