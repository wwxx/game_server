-module(alliance_applications_model).

-include("include/db_schema.hrl").

-export([info/1, load_data/1]).

info([AllianceApp, User]) ->
    PendList = [
        User#users.user_name,
        User#users.might,
        User#users.exploit,
        leaderboard:rank(might, db:objectid_to_binary_string(User#users.'_id')),
        User#users.last_login_time
    ],
    model_utils:info(AllianceApp, PendList).

load_data(PlayerID) ->
    db:where(#alliance_applications{user_id = PlayerID}).
