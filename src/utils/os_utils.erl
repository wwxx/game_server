-module(os_utils).

-export([get_ip/0]).

get_ip() ->
    {ok, [{IP, _, _}|_]} = inet:getif(),
    IP.
