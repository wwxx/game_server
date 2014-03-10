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


-module(player_data_tests).
-include_lib("eunit/include/eunit.hrl").
-include ("include/db_schema.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define (UDID, <<"eunit_self_udid">>). 

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
player_data_api_test_() ->
    % [{"Player Data API test.", ?setup(fun tests/1)}].
    {foreach, 
     fun start/0,
     fun stop/1,
     [
      fun true_test/1
      % fun create_test/1,
      % fun delete_test/1
      % fun update_test/1,
      % fun search_test/1,
      % fun count_test/1,
      % fun clean_test/1,
      % fun record_status_test/1,
      % fun data_persist_test/1
     ]}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    game_server:start().

stop(_Pid) ->
	game_server:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
true_test(_Pid) ->
    [?_assert(true)].

create_test(_Pid) ->
    NewUdid = <<"new_user_udid">>,
    NewName = <<"new_user_name">>,
    PlayerID = player_data:get_player_id(?UDID),

    player:proxy(PlayerID, player_data, delete, [PlayerID, #users{udid = NewUdid, name = NewName}]),
    player:proxy(PlayerID, player_data, create, [PlayerID, #users{udid = NewUdid}]),

    R = player_data:find(PlayerID, #users{udid = NewUdid}),

    [?_assert(erlang:is_binary(PlayerID)),
     ?_assertEqual(R#users.udid, NewUdid),
     ?_assertNotEqual(PlayerID, <<"">>)].

delete_test(_Pid) ->
    PlayerID = player_data:get_player_id(?UDID),
    player:proxy(PlayerID, player_data, delete, [PlayerID, #users{udid = ?UDID}]),
    R = player_data:find(PlayerID, #users{udid = ?UDID}),
    player_data:flush_to_mysql(),
    {ok, Users} = db:find_by(users, udid, ?UDID),
    [?_assertEqual(R, undefined),
     ?_assertEqual(Users, [])].
    

tests(_Pid) ->
    Udid = <<"eunit_test_udid">>,
    NewUdid = <<"new_user_udid">>,
    NewName = <<"new_user_name">>,
    PlayerID = player_data:get_player_id(Udid),

    player:proxy(PlayerID, player_data, delete, [PlayerID, #users{udid = NewUdid, name = NewName}]),
    player:proxy(PlayerID, player_data, create, [PlayerID, #users{udid = NewUdid}]),

    R = player_data:find(PlayerID, #users{udid = NewUdid}),

    [?_assert(erlang:is_binary(PlayerID)),
     ?_assertEqual(R#users.udid, NewUdid),
     ?_assertNotEqual(PlayerID, <<"">>)
    ].

% find_test() ->
%     PlayerID = player_data:get_player_id(Udid),
%     ?_


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
