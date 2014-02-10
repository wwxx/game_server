-module (users_model).

-export ([info/1, load_data/1, get_player_id/1]).

-include ("include/db_schema.hrl").
-include("include/game_constant.hrl").

-define(EMBED_MODELS, [research, task, bookmarks, heros]).

info([User]) ->
    model_utils:info(User).

get_player_id(Udid) ->
    case db:find_by(users, udid, Udid) of
        {ok, [Rec]} ->
            Rec#users.uuid;
        {ok, []} ->
            create_new_user(Udid)
    end.

load_data(PlayerID) ->
    db:find_by(users, uuid, PlayerID).

% TODO
create_new_user(Udid) ->
    Uuid = uuid_factory:gen(),
    db:create(#users{
        uuid=Uuid, 
        udid=Udid,
        name="Guest", 
        gem=0, 
        paid=0,
        created_at = time_utils:datetime(),
        updated_at = time_utils:datetime()
        }),
    Uuid.
