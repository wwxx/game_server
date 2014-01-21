%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2014, Savin-Max. All Rights Reserved.
%%% @doc
%%%        Callbacks invoked by GameServer for initializing
%%% @end
%%% Created :  二  1 21 14:58:43 2014 by Savin-Max µ

-module(life_cycle).

-export([before_start/0, after_start/0]).

%%%===================================================================
%%% Framework Callbacks
%%%===================================================================

%% before start game_server
before_start() ->
    %% add your custom initialize at here
    ok.

%% after start game_server
after_start() ->
    %% add your custom initialize at here
    model_mapping:load(),
    ok.

%%%===================================================================
%%% Private Custom initialize functions
%%%===================================================================

