-module(http).

-define(HTTP_CLIENT_TIMEOUT, 10000).
-define(HTTP_CLIENT_OPTIONS, [{max_sessions, 100}, {max_pipeline_size, 10}]).
-define(JSON_CONTENT, {"Content-Type", "application/json"}).

 
-export([request/1,
         post/2,
         post_data/2,
         get/1,
         async_post/2
         ]).
 
request(Url) ->
    ibrowse:send_req(Url, [?JSON_CONTENT], get, [], ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).   
 
get(Url) ->
    ibrowse:send_req(Url, [?JSON_CONTENT], get, [], ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).
 
post(Url, Body) ->
    ibrowse:send_req(Url, [?JSON_CONTENT], post, Body, ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).
 
%% This Like  curl -d "uid=111&pw=111&stage=yes" "http://127.0.0.1/test.php"
post_data(Url, Body) ->
    ibrowse:send_req(Url, [{"Content-Type", "application/x-www-form-urlencoded"}], post, Body, ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT).
 
async_post(Url, Body) ->
    ibrowse:send_req(Url, [], post, Body, ?HTTP_CLIENT_OPTIONS ++ [{stream_to, self()}], ?HTTP_CLIENT_TIMEOUT).
