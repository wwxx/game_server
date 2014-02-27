-module(secure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("secure.hrl").


secure_test_() ->
    Data = <<"hello">>,
    CipherData = secure:encrypt(?AES_KEY, ?AES_IVEC, <<"hello">>),
    DecriptedData = secure:decrypt(?AES_KEY, ?AES_IVEC, CipherData),
    [?_assertEqual(Data, DecriptedData)].

