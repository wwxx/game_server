-module(test_helper).
-export([assert_error_msg/2, assert_no_error_msg/1]).

-include_lib("eunit/include/eunit.hrl").

assert_error_msg(Response, ErrorAtom) ->
    case error_msg:const(ErrorAtom) of
        {fail, ErrorAtom} ->
            ?_assertEqual([{code, 0}, {desc, atom_to_binary(ErrorAtom, utf8)}], Response);
        ErrorCode ->
            ?_assertEqual(ErrorCode, proplists:get_value(code, Response))
    end.

assert_no_error_msg(Response) ->
    ?_assertNotMatch([{code, _}, {desc, _}], Response).
