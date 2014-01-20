%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
%%% @doc
%%%        常用游戏配置参数
%%% @end
%%% Created :  一 10 07 16:23:28 2013 by Savin-Max µ

%% NOTE 提交苹果审核的时候开启，审核通过后关闭
-define(SERVER_IN_APPLE_CHECK, true).

-define(NEW_TOWN_BASE_PRICE, 800).
-define(NEW_QUEUE_BASE_PRICE, 600).

%% error types
-define(NORMAL, 1).
-define(CHECK_TIME, 2).
-define(RELOAD_DATA, 3).
-define(INVALID_RECEIPT, 4).
-define(USE_PASSPORT_LOGIN, 5).
-define(RELOGIN, 6).
-define(INVALID_PASSWORD, 7).
-define(NOT_ENOUGH_GEMS, 8).
-define(CONCURRENT, 9).
-define(CHANGE_ACCOUNT, 10).
-define(DOWNLOAD_NEW_APP, 11).
-define(BAD_SIGNATURE, 12).
-define(NEED_MORE_BUILDER, 13).
-define(NEED_MORE_TRAINER, 14).
-define(NEED_MORE_RESEARCHER, 15).
-define(NEED_MORE_DISPATCHER, 16).
-define(NOT_ENOUGH_BUILDING_QUEUE, 17).
-define(NOT_ENOUGH_TRAINNING_QUEUE, 18).
-define(NOT_ENOUGH_RESEARCHING_QUEUE, 19).
-define(NOT_ENOUGH_DISPATCHING_QUEUE, 20).
-define(NOT_ENOUGH_RESOURCE, 21).
-define(BUFF_CHECKER_DELAY, 100).
-define(ERROR_NORMAL, 298).

%% letter category
-define(LETTER_CATEGORY_COMMON, 1).
-define(LETTER_CATEGORY_REPORT, 2).
-define(LETTER_CATEGORY_ALLIANCE, 3).
-define(LETTER_CATEGORY_FRIEND, 4).
%% letter type
-define(LETTER_PERSONAL, 1).
-define(LETTER_SYSTEM, 2).
-define(LETTER_ALLIANCE, 3).
-define(LETTER_ANNOUNCEMENT, 4).
-define(LETTER_ATTACK_REPORT, 5).
-define(LETTER_SPY_REPORT, 6).
-define(LETTER_DEFEND_REPORT, 7).
-define(LETTER_STRATEGY_REPORT, 8).
-define(LETTER_FRIEND, 9).

-define(BUY_SPY_REPORT_PRICE, 30).
-define(MAX_ENERGY_LIMIT, 9000).
-define(MIN_ENERGY_LIMIT, 4500).

%% town types
-define(TOWN_CAPITAL, 1).
-define(TOWN_NORMAL, 2).

-define(MIN_CAPACITY, 15000).

-define(MIN_PROTECT, 15000).
-define(ORE_BONUS, 0.05).
-define(RESOURCE_BASE_PRODUCE, 10).
-define(TOTAL_PERCENT, 100).
-define(MAX_EXPLORE_TIME, 10).
-define(MAX_EXPEDITION_TIME, 5).
-define(DEFAULT_EXPLORE_POSITION, 61).
-define(DEFAULT_EXPEDITION_POSITION, 62).

-define(MISSION_ATTACK, 1).
-define(MISSION_SPY, 2).
-define(MISSION_DEPLOY, 3).
-define(MISSION_TRANSPORT, 4).
-define(MISSION_EXPLORE, 5).
-define(MISSION_EXPEDITION, 6).
-define(WIN_SCORE, 30).
-define(DRAW_SCORE, 10).
-define(LOSE_SCORE, -10).
-define(RESEARCH_MAX_LEVEL, 30).
-define(BUILDING_MAX_LEVEL, 30).

-define(NORMAL_RESEARCH, 1).
-define(BATTLE_RESEARCH, 2).

-define(USER_MAX_TOWN_AMOUNT, 5).
-define(USER_MAX_RESEARCHER_AMOUNT, 2).

-define(ORE_EACH_LEVEL_BONUS, 5).
-define(NEXT_LOOKUP_BASIC_USE, 100).

%%NOTIFY_EXPIRE 30.minutes
-define(NOTIFY_EXPIRE, 86400).
-define(CHAT_WORLD, 1).
-define(CHAT_ALLIANCE, 2).
-define(CHATS_FIRST_AMOUNT, 6).
-define(LETTER_PUSH, 1).
-define(INITIAL_PRODUCE_VALUE, 200).
-define(INC_WORKER, 1).
-define(DEC_WORKER, 2).
-define(MAX_TOWN_NAME_SIZE, 12).
-define(TOWN_INITIAL_RESOURCE, 15000).
-define(INITIAL_WORKER_AMOUNT, 40).
-define(INITIAL_BUILDER_AMOUNT, 2).
-define(INITIAL_TRAINER_AMOUNT, 1).
-define(INITIAL_DISPATCHER_AMOUNT, 2).
-define(LEVEL_DIFF, 7).
-define(ARMOR_SCALE, 1).
-define(JOB_QUEUE_DELAY, 60).

%% production mode
-define(GATEWAY_INTERNAL_URL, <<"http://001.gateway.benmangguo.com/internal_apis">>).
%% development mode
%-define(GATEWAY_INTERNAL_URL, <<"http://42.96.138.143:4040/internal_apis">>).

-define(ACTIVITY_DAILY_TIMES_MEMBER, 1).
-define(ACTIVITY_DAILY_TIMES_PRESIDENT, 2).

-define(WIN_EXPLOIT_RATIO, 0.7).
-define(LOSE_EXPLOIT_RATIO, 0.3).
-define(DRAW_EXPLOIT_RATIO, 0.5).
-define(MIGHT_TO_EXPLOIT, 0.01).

-define(ACTIVITY_DURATION, 14400).
-define(ACTIVITY_PENDING, 0).
-define(ACTIVITY_PREPARE, 1).
-define(ACTIVITY_REINFORCING, 2).
-define(ACTIVITY_RUNNING, 3).
-define(ACTIVITY_FINISHED, 4).
-define(ACTIVITY_REFUSED, 5).
-define(ACTIVITY_TIMEOUT, 6).

-define(ACTIVITY_TYPE_COMMON, 0).
-define(ACTIVITY_TYPE_APPLY, 1).
-define(MAX_ACTIVITY_MISSION, 4).

-define(ACCOUNT_LEVEL_NORMAL, 0).
-define(ACCOUNT_LEVEL_GM, 1).


-define(MAP_ZONE_MIN, 1).
-define(MAP_ZONE_MAX, 10000).
-define(MAP_POSITION_MIN, 1).
-define(MAP_POSITION_MAX, 132).
-define(DISABLED_POSITIONS, [61,62,71,72,73,82,83,84]).
-define(ZONE_MAP_EMPTY_AMOUNT, 80).
-define(ACTIVITY_DEPLOY_SPEED_RATIO, 5.0).
-define(ALLIANCE_RESEARCH_MAX_LEVEL, 10).
-define(ALLIANCE_MEMBER_PER_LEVEL, 5).
-define(BOOKMARK_PLAYER, 0).
-define(BOOKMARK_NPC, 1).
-define(BOOKMARK_ORE, 2).
-define(HERO_BASE_RATE, 0.5).
-define(HERO_LEVEL_RATE, 0.05).
-define(HERO_CAN_UPGRADE_MAX_LEVEL, 9).
-define(HERO_FULL_LEVEL_PRICE, 300).
-define(HERO_MAX_LEVEL, 10).
-define(HERO_WITHOUT_ALLIANCE_MAX_LEVEL, 3).
-define(TECH_TREE_EXPIRE_LEVEL, 8).
-define(ZONE_MARCH_DURATION, 1).
-define(POSITION_MARCH_DURATION, 1).
-define(MAX_MARCH_DURATION, 21600).
-define(MIN_MARCH_DURATION, 30).
-define(PROTECTED_CITY_MAX_RESOURCE_AMOUNT, 45000000).
-define(ER_ATTACK_LIMIT_LEVEL, 7).
-define(ER_ATTACK_MIGHT_RATIO, 2.5).
-define(ER_ATTACK_MAX_MIGHT_LOSE_PERCENT, 0.11).

-define(TOWN_REINFORCE_IDLE, 0).
-define(TOWN_REINFORCE_COMBAT, 1).
-define(TOWN_REINFORCE_RETURN, 2).
-define(TOWN_REINFORCE_FINISH, 3).

-define(MIGHT_TO_HONOUR, 0.01).
-define(PLAYER, 0).
-define(NPC, 1).
-define(EXPLORE, 2).
-define(WILD_ENERGY, 3).
-define(WILD_METAL, 4).
-define(WILD_CRYSTAL, 5).
-define(WILD_GAS, 6).


-define(DEFAULT_ALLIANCE_EVENTS_AMOUNT, 20).
-define(ATTACKER_MIN_MIGHT_RATIO, 0.1).

-define(SUPPORT_MAIL, <<"tickets@benmangguo.uservoice.com">>).

-define(HELP_MAIL, <<"tickets@benmangguo.uservoice.com">>).
-define(EXPEDITION_MARCH_TIME, 180).
-define(MIGHT_PER_GEM, 5000).
-define(INJURED_RATE, 0.5).
-define(TOWN_REINFORCE_MAX_AMOUNT, 5).

-define(SLAUGHTER_LEVEL, 21).
-define(NORMAL_SPIN, 0).
-define(ADVANCE_SPIN, 1).
-define(NORMAL_SPIN_PRICE, 300).
-define(ADVANCE_SPIN_PRICE, 1800).
-define(STARTER_GEMS, 55).
-define(PROTECTED_CITY_MAX_RESOURCE_EXTRA, 5000000).
-define(USE_ERLANG_PUSH, true).
-define(RESEARCH_DISPATCH_PERCENT, 0.03).
-define(MAX_TRAINER_AMOUNT, 2).
-define(MAX_DISPATCHER_AMOUNT, 5).
-define(MAX_BUILDER_AMOUNT, 5).


