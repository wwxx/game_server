-module(mail).

-export([send/4, exception_notify/1]).

-define(DEVELOPER_EMAILS, "mafei.198@gmail.com").

exception_notify(ExceptionMsg) ->
    send(inet:gethostname(), ?DEVELOPER_EMAILS, "Server_Exception", ExceptionMsg).

send(From, To, Subject, Body) ->
    Content = io_lib:format("Subject: ~s \r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s", [Subject, From, To, Body]), 
    gen_smtp_client:send({From, [To], Content}, [{relay, "localhost"}, {port, 25}]).


% send(Subject, Body, Destination) ->
%     Cmd = "echo " ++ "\"" ++ Body ++ "\"" ++ " | mail -a \"Content-Transfer-Encoding: BASE64;\"" ++ " -s " ++ "\"" ++ Subject ++ "\"" ++ " " ++ Destination,
%     os:cmd(Cmd).

% send_test() ->
%     Destination = "mafei.198@gmail.com savin.notify@gmail.com",
%     Subject = "hello",
%     Body = "Hello guys",
%     send(Subject, Body, Destination).
