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
            create_new_user(PlayerID);
        OldUser ->
            OldUser
    end,
    login(PlayerID, User).

login(PlayerID, User) ->
    ok.
    %Bookmarks = player_data:where(PlayerID, #bookmarks{user_id=PlayerID}),
    %Research = player_data:find(PlayerID, #researches{user_id=PlayerID}),
    %Task = player_data:find(PlayerID, #tasks{user_id=PlayerID}),
    %Heros = player_data:where(PlayerID, #heros{user_id=PlayerID}),
    %AllianceApps = player_data:where(PlayerID, #alliance_applications{user_id=PlayerID}),
    %Item = player_data:find(PlayerID, #items{user_id=PlayerID}),
    %Events = player_data:where(PlayerID, #events{user_id=PlayerID, finished=false}),
    %Towns = player_data:where(PlayerID, #towns{user_id=PlayerID, is_robot=false}),
    %FriendList = player_data:find(PlayerID, #friend_lists{user_id=PlayerID}),
    %Blacklists = player_data:where(PlayerID, #blacklists{user_id=PlayerID}),

    %[
        %{users, [record:attributes(User, [research, task, bookmarks, heros])]},
        %{alliance_applications, ?infos(AllianceApps)},
        %{items, ?infos([Item])},
        %{events, ?infos(Events)},
        %{researches, ?infos([Research])},
        %{bookmarks, ?infos(Bookmarks)},
        %{tasks, ?infos([Task])},
        %{heros, ?infos(Heros)},
        %{towns, record:map_attributes(Towns, [buff])},
        %{blacklists, ?infos(Blacklists)},
        %{friend_lists, ?infos([FriendList])},
        %{unread_letter_counts, letters_model:unread_letter_counts(PlayerID)},
        %{current_activity_id, users_model:current_activity_id(PlayerID)},
        %{remain_explore_times, explore_model:remain_amount(PlayerID)},
        %{remain_expedition_times, expedition_model:remain_amount(PlayerID)},
        %{buy_more_explore_price, explore_model:buy_price(PlayerID)},
        %{buy_more_expedition_price, expedition_model:buy_price(PlayerID)},
        %{server_time, time_utils:current_time()}
    %].

create_new_user(_PlayerID) ->
    ok.
