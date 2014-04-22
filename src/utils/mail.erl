-module(mail).

-export([send/3, exception_notify/1, send_test/0]).

-define(DEVELOPER_EMAILS, "mafei.198@gmail.com").

exception_notify(ExceptionMsg) ->
    send("Server_Exception", ExceptionMsg, ?DEVELOPER_EMAILS).

send(Subject, Body, Destination) ->
    Cmd = "echo " ++ "\"" ++ Body ++ "\"" ++ " | mail -a \"Content-Transfer-Encoding: BASE64;\"" ++ " -s " ++ "\"" ++ Subject ++ "\"" ++ " " ++ Destination,
    os:cmd(Cmd).

send_test() ->
    Destination = "mafei.198@gmail.com savin.notify@gmail.com",
    Subject = "hello",
    Body = "Hello guys",
    send(Subject, Body, Destination).
