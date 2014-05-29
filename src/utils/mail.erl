-module(mail).

-export([send/4, exception_notify/1]).

-define(DEVELOPER_EMAILS, "mafei.198@gmail.com").

exception_notify(ExceptionMsg) ->
    {ok, From} = inet:gethostname(),
    send(From, ?DEVELOPER_EMAILS, "Server_Exception", ExceptionMsg).

send(From, To, Subject, Body) ->
    Content = io_lib:format("Subject: ~s \r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]), 
    gen_smtp_client:send({From, [To], Content}, [{relay, "localhost"}, {port, 25}]).
