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

-module(fake_client).
-export([login/0,
         login/1,
         connect/0,
         login_req/0,
         % bench/1,
         request/3,
         send_request/3,
         recv_response/1]).

-include("../app/include/secure.hrl").

connect() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5555,
                                 [{active, false}, {packet, 2}]),
    put(sock, Sock).

login_req() ->
    Sock = get(sock),
    send_request(login_params, Sock, {<<"test_udid">>}),
    _Response = recv_response(Sock).

login(Udid) ->
    connect(),
    Sock = get(sock),
    send_request(login_params, Sock, {list_to_binary(Udid)}),
    Response = recv_response(Sock),
    ok = gen_tcp:close(Sock),
    Response.

login() ->
    connect(),
    Sock = get(sock),
    send_request(login_params, Sock, {<<"test_udid">>}),
    Response = recv_response(Sock),
    ok = gen_tcp:close(Sock),
    Response.

% bench(FakeClientAmount) ->
%     login(FakeClientAmount).

% login(0) -> ok;
% login(N) ->
%     SomeHostInNet = "localhost", % to make it runnable on one machine
%     {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5555,
%                                  [{active, false}, {packet, 2}]),
%     Udid = io_lib:format("fake_client_udid_~p", [N]),
%     send_request(login_params, Sock, {list_to_binary(Udid)}),
%     _Response = recv_response(Sock),
%     ok = gen_tcp:close(Sock),
%     login(N-1).

request(Udid, Protocol, Params) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5555,
                                 [{active, false}, {packet, 2}]),
    send_request(login_params, Sock, {Udid}),
    _LoginResponse = recv_response(Sock),
    %error_logger:info_msg("LoginResponse: ~p~n", [LoginResponse]),
    send_request(Protocol, Sock, Params),
    Response = recv_response(Sock),
    %error_logger:info_msg("Response: ~p~n", [Response]),
    ok = gen_tcp:close(Sock),
    Response.

send_request(Path, Sock, Value) ->
    Data = api_encoder:encode(Path, Value),
    gen_tcp:send(Sock, secure:encrypt(Data)).

recv_response(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Packet} ->
            Data = secure:decrypt(Packet),
            {Response, _LeftData} = api_decoder:decode(Data),
            %error_logger:info_msg("Response: ~p~n", [Response]),
            Response;
        Error -> error_logger:info_msg("Error Response: ~p~n", [Error])
    end.
