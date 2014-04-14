-module(game_counter).

-export([start/0, gen_id/1,
         incr_daily_action/1,
         incr_daily_action/2,
         get_daily_action/1,
         clean_daily_counters/0,
         get/1, set/2, del/1]).

-record(mapper, {name, value}).
-record(daily_counter, {name, value}).
-record(counter, {name, value}).

start() ->
    mnesia:create_table(mapper, [{disc_copies, [node()]},
                                 {type, set},
                                 {attributes, record_info(fields, mapper)}]),
    mnesia:create_table(counter, [{disc_copies, [node()]},
                                  {type, set},
                                  {attributes, record_info(fields, counter)}]),
    create_table_daily_counter(),
    %% delete old daily counter table and reinit at the begining of tomorrow.
    MFA = {game_counter, clean_daily_counters, []},
    timertask:add(clean_daily_counter, time_utils:remain_seconds_to_tomorrow(), MFA).

create_table_daily_counter() ->
    mnesia:create_table(daily_counter, [{disc_copies, [node()]},
                                        {type, set},
                                        {attributes, record_info(fields, daily_counter)}]).

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

incr_daily_action(Name) ->
    mnesia:dirty_update_counter(daily_counter, {Name, date()}, 1).

incr_daily_action(Name, Amount) ->
    mnesia:dirty_update_counter(daily_counter, {Name, date()}, Amount).

get_daily_action(Name) ->
    case mnesia:dirty_read(daily_counter, {Name, date()}) of
        [] -> 0;
        [Rec] -> Rec#daily_counter.value
    end.

clean_daily_counters() ->
    mnesia:delete_table(daily_counter),
    create_table_daily_counter().
