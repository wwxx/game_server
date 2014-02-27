%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2014
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

%%% @doc
%%%        登陆、退出
%%% @end
%%% Created :  一 10 07 11:46:09 2013 by Savin-Max µ

-module(sessions_controller).

-export([login/1]).

-include("include/db_schema.hrl").
-include("include/game_constant.hrl").

-define(info(Rec), record:attributes(Rec)).
-define(infos(Recs), record:map_attributes(Recs)).

login(PlayerID) ->
    RecordSelector = #users{'uuid' = PlayerID},
    User = case player_data:find(PlayerID, RecordSelector) of
        undefined ->
            io:format("User Not Found!~n");
        OldUser ->
            io:format("OldUser: ~p~n", [OldUser]),
            OldUser
    end,
    login(PlayerID, User).

login(_PlayerID, User) ->
    Value = { User#users.uuid,
              User#users.udid,
              User#users.name,
              User#users.gem,
              User#users.paid },
    response_encoder:encode(user, Value).
