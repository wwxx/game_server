-module(fake_client).
-compile(export_all).

-include("include/secure.hrl").


client() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5555,
                                 [{active, false}, {packet, 4}]),
    Data = list_to_binary([utils_protocol:encode_integer(1), utils_protocol:encode_string(<<"test_udid">>)]),
    ok = gen_tcp:send(Sock, encrypt(Data)),
    {ok, Packet} = gen_tcp:recv(Sock, 0),
    Response = decrypt(Packet),
    io:format("Response: ~p~n", [Response]),
    ok = gen_tcp:close(Sock).

encrypt(Data) ->
    secure:encrypt(?AES_KEY, ?AES_IVEC, Data).

decrypt(Data) ->
    secure:decrypt(?AES_KEY, ?AES_IVEC, Data).
