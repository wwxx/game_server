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

-module (users_model).

-export ([info/1, load_data/1, get_player_id/1]).

-include ("include/db_schema.hrl").
-include("include/game_constant.hrl").

-define(EMBED_MODELS, [research, task, bookmarks, heros]).

info([User]) ->
    model_utils:info(User).

get_player_id(Udid) ->
    case db:find_by(users, udid, Udid) of
        {ok, [Rec]} ->
            io:format("Found user with udid: ~p~n", [Udid]),
            Rec#users.uuid;
        {ok, []} ->
            io:format("Create new user with udid: ~p~n", [Udid]),
            create_new_user(Udid)
    end.

load_data(PlayerID) ->
    db:find_by(users, uuid, PlayerID).

% TODO
create_new_user(Udid) ->
    Uuid = uuid_factory:gen(),
    db:create(#users{
        uuid=Uuid,
        udid=Udid,
        name=list_to_binary(["Guest_", Udid]),
        gem=0,
        paid=0,
        created_at = time_utils:datetime(),
        updated_at = time_utils:datetime()
        }),
    init_heros(Uuid),
    Hero = player_data:find(Uuid, #heros{user_id = Uuid}),
     init_formation(Uuid, Hero#heros.uuid),
    Uuid.

init_heros(PlayerID) ->
    % ConfigId = game_numerical:find_element(config_player, 1, 2),
    Uuid = uuid_factory:gen(),
    Hero = #heros{uuid = Uuid,
                  user_id = PlayerID,
                  % config_id = ConfigId,
                  config_id = 1,
                  level = 1,
                  created_at = time_utils:datetime(),
                  updated_at = time_utils:datetime()
    },
    io:format("Hero: ~p~n", [Hero]),
    db:create(Hero).

init_formation(PlayerID, HeroId) ->
    Uuid = uuid_factory:gen(),
    Formation = #formations{uuid = Uuid,
                            user_id = PlayerID,
                            matrix = [HeroId, <<"">>, <<"">>, <<"">>, <<"">>, <<"">>, <<"">>, <<"">>, <<"">>]},
    db:create(Formation).
