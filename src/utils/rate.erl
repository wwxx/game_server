-module(rate).
-export([happen/1, range/1]).

happen(Rate) ->
    RandomValue = random:uniform(10000),
    compare(RandomValue, Rate).

%% Range = [{RateA, ValueA}, {RateB, ValueB}, {default, DefaultValue}],
%% select the happened Rate 
range(Range) ->
    select(random:uniform(10000), Range).

select(RandomValue, [{Rate, Value}|_Range]) when RandomValue =< Rate -> Value;
select(_RandomValue, [{default, Value}]) -> Value;
select(RandomValue, [{_Rate, _Value}|Range]) ->
    select(RandomValue, Range).

compare(A, B) when A < B -> true;
compare(_A, _B) -> false.
