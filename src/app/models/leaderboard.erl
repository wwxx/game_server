%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
%%% @doc
%%%        Leaderboard for ranking players
%%% @end
%%% Created :  一 10 07 18:56:53 2013 by Savin-Max µ

-module(leaderboard).

-export([rank/2,
         incr_score/3,
         decr_score/3]).

rank(wealth, PlayerID) ->
    1;
rank(might, PlayerID) ->
    1;
rank(honour, PlayerID) ->
    1.

incr_score(wealth, PlayerID, Amount) ->
    ok;
incr_score(might, PlayerID, Amount) ->
    ok;
incr_score(honour, PlayerID, Amount) ->
    ok.

decr_score(wealth, PlayerID, Amount) ->
decrok;
decr_score(might, PlayerID, Amount) ->
decrok;
decr_score(honour, PlayerID, Amount) ->
    ok.
