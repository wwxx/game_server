-module(events_model).

-export([info/1, load_data/1]).

-include("include/db_schema.hrl").

info([Event]) ->
    RemainSeconds = Event#events.finish_at - time_utils:current_time(),
    PendList = [RemainSeconds],
    model_utils:info(Event, PendList).

load_data(PlayerID) ->
    db:where(#events{finished = 0, user_id = PlayerID}).
