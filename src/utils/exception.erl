-module(exception).
-export([notify/2, format/2]).

format(Type, Msg) ->
    io_lib:format("~p, ~p~n ~p", [Type, Msg, erlang:get_stacktrace()]).

notify(ErrorType, ErrorMsg) ->
    Stacktrace = format(ErrorType, ErrorMsg),
    error_logger:error_msg("Caught Exception: ~s~n", [Stacktrace]),
    case application:get_env(game_server, server_environment) of
        {ok, production} ->
            mail:exception_notify(Stacktrace);
        _ -> do_nothing
    end.
