%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2024
%%% Savin Max <mafei.198@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
%%% @doc
%%%        Player progress will call this module when it received gprocs broadcast.
%%% @end
%%% Created :  四  3 13 14:18:36 2014 by Savin Max

-module(player_subscribe).

%% Player Module Callback
-export([handle/3]).

handle(Channel, PlayerID, Msg) ->
    %% dispatch to your custom msg handler.
    handle_msg(PlayerID, Channel, Msg).

handle_msg({world_chat, _ServerId}, PlayerID, Msg) ->
    player:send_data(PlayerID, api_encoder:encode(world_chat, Msg));
handle_msg({alliance_chat, _AllianceId}, PlayerID, Msg) ->
    player:send_data(PlayerID, api_encoder:encode(alliance_chat, Msg));
handle_msg(Channel, PlayerID, Msg) ->
    error_logger:info_msg("Unhandled Msg! PlayerID: ~p, Channel: ~p, Msg: ~p~n",
                          [Channel, PlayerID, Msg]).
