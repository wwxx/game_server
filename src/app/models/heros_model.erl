-module(heros_model).

-export([load_data/1]).

-include("include/db_schema.hrl").

load_data(PlayerID) ->
    {ok, User} = db:find_one(#users{'_id' = PlayerID}, #users{heros = 1}),
    if
        User#users.heros =:= undefined ->
            {ok, []};
        true ->
            Heros = lists:map(fun(Hero) ->
                Proplist = [user_id, PlayerID|tuple_to_list(Hero)],
                Document = list_to_tuple(Proplist),
                io:format("Document: ~p~n", [Document]),
                record_mapper:unmap(heros, Document)
            end, User#users.heros),
            {ok, Heros}
    end.
