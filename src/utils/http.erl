-module(http).

%% Connection pool size: 100, Each connection queue size: 100
-define(HTTP_CLIENT_OPTIONS, [{max_sessions, 100}, {max_pipeline_size, 100}]).

-define(HTTP_CLIENT_TIMEOUT, 10000).
-define(JSON_CONTENT, {"Content-Type", "application/json"}).
 
-export([request/3,
         async_request/3]).
 
request(Url, Method, Params) ->
    request(Url, Method, Params, false).

async_request(Url, Method, Params) ->
    request(Url, Method, Params, true).

request(Url, Method, Params, IsAsync) ->
    Options = case IsAsync of
                  true -> ?HTTP_CLIENT_OPTIONS ++ [{stream_to, self()}];
                  false -> ?HTTP_CLIENT_OPTIONS
              end,
    JsonString = case Params of
                     [] -> Params;
                     _  -> jsx:encode(Params)
                 end,
    ibrowse:send_req(Url, [?JSON_CONTENT], Method, JsonString, Options, ?HTTP_CLIENT_TIMEOUT).   
