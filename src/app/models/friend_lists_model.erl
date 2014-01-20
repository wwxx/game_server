-module(friend_lists_model).

-export([load_data/1]).

-include("include/db_schema.hrl").

load_data(PlayerID) ->
    db:where(#friend_lists{user_id = PlayerID}).
