%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.


-module(time_utils).

-export([now/0,
         current_time/0,
         remain_seconds_to_tomorrow/0,
         end_of_today/0,
         current_time_to_now/1,
         time_to_seconds/3,
         datetime/0,
         to_i/1]).

now() ->
    current_time().

current_time() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

remain_seconds_to_tomorrow() ->
    end_of_today() - current_time().

end_of_today() ->
    calendar:datetime_to_gregorian_seconds({date(),{24,0,0}}) - 62167219200.

current_time_to_now(CurrentTime) ->
    MegaSecs = CurrentTime div 1000000,
    Secs = CurrentTime rem 1000000,
    {MegaSecs, Secs, 0}.

time_to_seconds(MegaSecs, Secs, _MicroSecs) ->
    MegaSecs * 1000000 + Secs.

datetime() ->
    {datetime, {erlang:date(), erlang:time()}}.

to_i({datetime, {Date, Time}}) ->
    calendar:datetime_to_gregorian_seconds({Date, Time})  - 62167219200.
