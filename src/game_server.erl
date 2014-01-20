%% Feel free to use, reuse and abuse the code in this file.

-module(game_server).

%% API.
-export([start/0]).

%% API.

start() ->
    io:format("Game Server Starting~n"),
    ok = lager:start(), %% Logger
    ok = application:start(sync), %% Hot reload code
    ok = application:start(bson), %% MongoDB dependency
    ok = application:start(gproc), %% Process dictionary
    ok = application:start(yamerl), %% yaml loader and writer
    ok = application:start(crypto),
    ok = application:start(mongodb), %% MongoDB
    ok = application:start(mongrel), %% MongoDB sql wrapper
    ok = application:start(game_server). %% Game Server
