-module(after_game_server_start).

-export([setup/0]).

%%% ---------------------------------------------------------------------------
%%% Framework callback, be called at game server started!
%%% ---------------------------------------------------------------------------
setup() ->
    % Do some custom initializes by yourself!
    ok.
