%%%===================================================================
%%% Generated by generate_api.rb 2014-02-28 21:08:32 +0800
%%%===================================================================
-module(routes).
-export([route/1]).

route(3) ->
    {sessions_controller, login};
route(4) ->
    {users_controller, public_info}.
