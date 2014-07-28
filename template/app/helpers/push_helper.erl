-module(push_helper).
-export([push/2, push/3, 
         send_msg/2, batch_send_msg/2]).
-include("include/db_schema.hrl").

push(UserIds, Text) ->
    lists:foreach(fun(UserId) ->
        player:async_wrap(UserId, fun() ->
            User = model:find(#users{uuid = UserId}),
            send_msg(User#users.device_token, Text)
        end)
    end, UserIds).

push(UserIds, PushId, Values) ->
    lists:foreach(fun(UserId) ->
        player:async_wrap(UserId, fun() ->
            User = model:find(#users{uuid = UserId}),
            Text = i18n:t(config_pushes, PushId, User#users.locale, Values),
            send_msg(User#users.device_token, Text)
        end)
    end, UserIds).

send_msg(undefined, _Text) -> ok;
send_msg(DeviceToken, Text) -> 
    apns:send_message(push, binary_to_list(DeviceToken), Text).

batch_send_msg(DeviceTokens, Text) ->
    lists:foreach(fun(DeviceToken) ->
        apns:send_message(push, binary_to_list(DeviceToken), Text)
    end, DeviceTokens).
