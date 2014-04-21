-module(for).

-export([times/2, each/2]).

times(0, _) -> ok;
times(N, Fun) ->
    Fun(),
    times(N - 1, Fun).

each([], _) -> ok;
each([Item|List], Fun) ->
    Fun(Item),
    each(List, Fun).

