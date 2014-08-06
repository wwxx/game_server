-module(mail).

-export([send/4, exception_notify/1]).

-define(DEVELOPER_EMAILS, ["mafei.198@gmail.com", "abtree@qq.com"]).

exception_notify(ExceptionMsg) ->
    {ok, From} = inet:gethostname(),
    send(From, ?DEVELOPER_EMAILS, "Server_Exception", ExceptionMsg).

send(From, Receivers, Subject, Body) ->
    To = lists:foldl(fun(Receiver, Acc) ->
                         if
                             Acc =:= "" ->
                                 Receiver;
                             true ->
                                 lists:concat([Acc, ", ", Receiver])
                         end
                         
                     end, "", Receivers),
    Content = io_lib:format("Subject: ~s \r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", 
                            [Subject, From, To, Body]), 
    gen_smtp_client:send({From, Receivers, Content}, [{relay, "localhost"}, {port, 25}]).
