%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
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
