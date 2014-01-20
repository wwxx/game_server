%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
%%% @doc
%%%        Route requests to the matched Module & Function
%%% @end
%%% Created :  六 10 12 01:45:07 2013 by Savin-Max µ

-module(routes).

-export([match/1]).

-define(ROUTES, [
        {<<"sessions/login">>, {sessions_controller, login}}
        ]).


match(Path) ->
    proplists:get_value(Path, ?ROUTES).
