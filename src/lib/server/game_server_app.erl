%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(game_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    io:format("pusher_app start"),
	game_server_sup:start_link().

stop(_State) ->
	ok.
