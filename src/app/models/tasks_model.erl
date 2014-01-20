-module(tasks_model).

-export([load_data/1]).

-include("include/db_schema.hrl").

load_data(PlayerID) ->
    {ok, User} = db:find_one(#users{'_id' = PlayerID}, #users{task = 1}),
    Proplist = [user_id, PlayerID|tuple_to_list(User#users.task)],
    Document = list_to_tuple(Proplist),
    Task = record_mapper:unmap(tasks, Document),
    {ok, [Task]}.
