-module(game_counter).

-export([gen_id/1, start/0]).

-record (counter, {name, value}).

start() ->
    Nodes = [node()],
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(counter, [{disc_copies, Nodes},
                                  {type, set},
                                  {attributes, record_info(fields, counter)}]).

gen_id(Name) ->
    mnesia:dirty_update_counter(counter, Name, 1).
