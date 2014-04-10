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

-module(load_test).
-export([b/3, bench/2, summary/0, d/1]).

-include("../app/include/secure.hrl").

-define(TAB, ?MODULE).


%% C: 并发客户端数量
%% N: 每个客户端发送请求数量
%% I: 客户端请求发送间隔时间
d(C) ->
    b(C, 100, 300).

b(C, N, I) ->
    case ets:info(?TAB) of
        undefined -> do_nothing;
        _ -> ets:delete(?TAB)
    end,
    ets:new(?TAB, [set, public, named_table]),
    ets:insert(?TAB, {count, 0}),
    ets:insert(?TAB, {msecs, 0}),
    ets:insert(?TAB, {c, C}),
    ets:insert(?TAB, {n, N}),
    ets:insert(?TAB, {error, 0}),
    ets:insert(?TAB, {number, 0}),
    times(C, fun() -> spawn(load_test, bench, [N, I]) end).

times(0, _F) -> ok;
times(N, F) ->
    timer:sleep(10),
    F(),
    times(N - 1, F).

bench(N, I) ->
    SomeHostInNet = "127.0.0.1", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5555,
                                 [{active, false}, {packet, 2}]),
    StartTimeStamp = os:timestamp(),

    Counter = ets:update_counter(?TAB, number, 1),
    UdidStr = io_lib:format("load_test_udid_~p", [Counter]),
    Udid = list_to_binary(UdidStr),
    % player_data:get_player_id(Udid),
    fake_client:send_request(login_params, Sock, {Udid}),

    run(N, I, Sock, Udid),
    StopTimeStamp = os:timestamp(),
    result(StartTimeStamp, StopTimeStamp),
    exit(normal).

run(0, _I, Sock, _Udid) -> gen_tcp:close(Sock);
run(N, I, Sock, Udid) ->
    if
        I > 0 ->
            timer:sleep(I);
        true ->
            do_nothing
    end,
    fake_client:send_request(formation_info_params, Sock, {}),
    % gen_tcp:send(Sock, "hello, i'm erlang client!!!!!!!!!!!!!!!!!!!"),
    case gen_tcp:recv(Sock, 0) of
        {ok, _Packet} -> do_nothing;
        _Error -> ets:update_counter(?TAB, error, 1)
    end,
    ets:update_counter(?TAB, count, 1),
    % io:format("Response: ~p~n", [Res]),
    run(N-1, I, Sock, Udid).

result(StartTimeStamp, StopTimeStamp) ->
    {_StartMegaSecs, StartSecs, StartMicroSecs} = StartTimeStamp,
    {_StopMegaSecs, StopSecs, StopMicroSecs} = StopTimeStamp,
    UsedMicroSecs = StopMicroSecs - StartMicroSecs + (StopSecs - StartSecs) * 1000000,
    ets:update_counter(?TAB, msecs, UsedMicroSecs),
    io:format("ok~n").

summary() ->
    [{c, C}] = ets:lookup(?TAB, c),
    [{n, N}] = ets:lookup(?TAB, n),
    [{error, Error}] = ets:lookup(?TAB, error),
    [{count, Count}] = ets:lookup(?TAB, count),
    [{msecs, UsedMicroSecs}] = ets:lookup(?TAB, msecs),
    io:format("Used: ~pus~n", [UsedMicroSecs / C]),
    io:format("Used: ~ps~n", [UsedMicroSecs / 1000000 / C]),
    MicroSecsPerRequest = UsedMicroSecs / Count / C,
    io:format("Total Request: ~p~n", [C * N]),
    io:format("Successed Request: ~p~n", [Count]),
    io:format("Error Request: ~p~n", [Error]),
    io:format("MicroSecsPerRequest: ~p~n", [MicroSecsPerRequest]),
    io:format("Requests Per Seconds: ~p~n", [1000000 / MicroSecsPerRequest]).
