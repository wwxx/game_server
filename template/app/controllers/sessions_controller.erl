-module(sessions_controller).

-export([login/2]).

-include("include/db_schema.hrl").
-include ("../app/include/game_const.hrl").

login(PlayerID, {_Udid}) ->
    User = users_model:current(),
    %% Join World Chat!
    chat_channel:join(PlayerID, {world_chat, server_id_001}),
    {user, users_model:info(User)}.

%%=========================================================
%% Internal functions
%%=========================================================
