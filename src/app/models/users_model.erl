-module (users_model).

-export ([info/1, embed_models/0, load_data/1, current_activity_id/1, get_player_id/1]).

-include ("include/db_schema.hrl").
-include("include/game_constant.hrl").

-define(EMBED_MODELS, [research, task, bookmarks, heros]).

info([User]) ->
    model_utils:info(User).

%TODO
current_activity_id(PlayerID) ->
    <<"fake_id">>.

next_city_price(TownsAmount) ->
    ?NEW_TOWN_BASE_PRICE * math:pow(2, TownsAmount - 1).

next_researcher_price(ResearchAmount) ->
    ?NEW_QUEUE_BASE_PRICE * math:pow(2, ResearchAmount - 1).

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
    db:where(#users{'uuid' = PlayerID}).

%TODO
create_new_user(Udid) ->
    ok.
