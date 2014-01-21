-module (users_model).

-export ([info/1, embed_models/0, load_data/1, get_player_id/1]).

-include ("include/db_schema.hrl").
-include("include/game_constant.hrl").

-define(EMBED_MODELS, [research, task, bookmarks, heros]).

info([User]) ->
    model_utils:info(User).

get_player_id(Udid) ->
    User = case db:where(#users{uuid = Udid}) of
        {ok, [Rec]} ->
            Rec;
        {ok, []} ->
            create_new_user(Udid)
    end,
    User#users.'uuid'.

embed_models() ->
    ?EMBED_MODELS.

load_data(PlayerID) ->
    db:find_by(users, uuid, PlayerID).

%TODO
create_new_user(Udid) ->
    Udid.
