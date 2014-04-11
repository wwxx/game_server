-module(game_counter).

-export([start/0, gen_id/1, 
         get/1, set/2, del/1]).

-record(counter, {name, value}).
-record(mapper, {name, value}).

start() ->
    mnesia:create_table(mapper, [{disc_copies, [node()]},
                                 {type, set},
                                 {attributes, record_info(fields, mapper)}]),
    mnesia:create_table(counter, [{disc_copies, [node()]},
                                  {type, set},
                                  {attributes, record_info(fields, counter)}]).

gen_id(Name) ->
    mnesia:dirty_update_counter(counter, Name, 1).

%% Mapper Methods
get(Name) ->
    case mnesia:dirty_read(mapper, Name) of
        [] -> undefined;
        [Mapper] -> Mapper#mapper.value
    end.

set(Name, Value) ->
    mnesia:dirty_write(mapper, #mapper{name=Name, value=Value}).

del(Name) ->
    mnesia:dirty_delete(mapper, Name).
