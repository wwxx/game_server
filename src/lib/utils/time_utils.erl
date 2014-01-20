-module(time_utils).

-compile(export_all).

current_time() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

time_to_seconds(MegaSecs, Secs, MicroSecs) ->
    MegaSecs * 1000000 + Secs.
