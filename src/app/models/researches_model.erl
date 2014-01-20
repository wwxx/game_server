-module(researches_model).

-export([infos/1, load_data/1]).

-include("include/db_schema.hrl").

infos(Research) ->
    model_utils:info(Research).

load_data(PlayerID) ->
    {ok, User} = db:find_one(#users{'_id' = PlayerID}, #users{research = 1}),
    Proplist = [user_id, PlayerID|tuple_to_list(User#users.research)],
    Document = list_to_tuple(Proplist),
    Research = record_mapper:unmap(researches, Document),
    {ok, [Research]}.
