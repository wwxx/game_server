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


-module(player_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../app/include/secure.hrl").
-include ("include/db_schema.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [fun update_test/1,
      fun find_test/1,
      fun delete_test/1,
      fun create_test/1,
      fun select_test/1
     ]}.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    game_server:start([test]),
    db:delete_all(towns),
    db:delete_all(users),
    db:delete_all(buildings).

stop(_Pid) ->
    game_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
update_test(_Pid) ->
    PlayerID = player_data:get_player_id(<<"eunit_test_udid">>),
    error_logger:info_msg("----------All: ~p~n", [player:proxy(PlayerID, model, all, [buildings])]),
    player_data:update(PlayerID, #buildings{user_id = PlayerID}, #buildings{name = <<"NewCastle">>}),
    Building = player_data:find(PlayerID, #buildings{name = <<"NewCastle">>}),
    [?_assertEqual(Building#buildings.name, <<"NewCastle">>)].

find_test(_Pid) ->
    PlayerID = player_data:get_player_id(<<"eunit_test_udid">>),
    Castle = player_data:find(PlayerID, #buildings{user_id = PlayerID}),
    error_logger:info_msg("Castle: ~p~n", [Castle]),
    error_logger:info_msg("Uuid: ~p~n", [Castle#buildings.uuid]),
    Building = player_data:find(PlayerID, #buildings{uuid = Castle#buildings.uuid}),
    Building2 = player_data:find(PlayerID, #buildings{user_id = PlayerID}),
    error_logger:info_msg("Building: ~p, Building2: ~p~n", [Building, Building2]),
    [?_assertEqual(Castle, Building)].

delete_test(_Pid) ->
    PlayerID = player_data:get_player_id(<<"eunit_test_udid">>),
    Castle = player_data:find(PlayerID, #buildings{user_id = PlayerID}),
    player_data:delete(PlayerID, #buildings{uuid = Castle#buildings.uuid}),
    Building = player_data:find(PlayerID, #buildings{uuid = Castle#buildings.uuid}),
    [?_assertEqual(undefined, Building)].

create_test(_Pid) ->
    PlayerID = player_data:get_player_id(<<"eunit_test_udid">>),
    player_data:create(PlayerID, #buildings{name = <<"house">>, user_id = PlayerID}),
    player_data:create(PlayerID, #buildings{name = <<"barrack">>, user_id = PlayerID}),
    Houses = player_data:where(PlayerID, #buildings{name = <<"house">>}),
    Barracks = player_data:where(PlayerID, #buildings{name = <<"barrack">>}),
    [?_assertEqual(length(Houses), 1),
     ?_assertEqual(length(Barracks), 1)].

select_test(_Pid) ->
    PlayerID = player_data:get_player_id(<<"eunit_test_udid">>),
    Buildings = player_data:where(PlayerID, #buildings{user_id = PlayerID}),
    [?_assertEqual(length(Buildings), 1)].


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
