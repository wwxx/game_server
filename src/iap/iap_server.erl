-module(iap_server).

-behaviour(poolboy_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([verify_receipt/3]).

-define(SERVER, ?MODULE).
-define(RESEND_RECEIPT_MILLISECONDS, 15000).
-define(HTTP_CLIENT_TIMEOUT, 10000).

-define(APP_SANDBOX_VERIFY_RECEIPT_URL, "https://sandbox.itunes.apple.com/verifyReceipt").
-define(APP_VERIFY_RECEIPT_URL, "https://buy.itunes.apple.com/verifyReceipt").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

verify_receipt(Id, Receipt, Callback) ->
    poolboy:transaction(iap_verify_worker_pool, fun(Worker) ->
        gen_server:cast(Worker, {verify_receipt, Id, Receipt, Callback})
    end).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% 发校验
handle_cast({verify_receipt, Id, Receipt, Callback}, State) ->
    case get({verify_receipt, Id}) of
        undefined ->
            do_verify_receipt(?APP_VERIFY_RECEIPT_URL, Id, Receipt, Callback);
        _ -> ok
    end,
    {noreply, State}.

%% 重发机制
handle_info({resend_receipt_data}, State) ->
    try 
        lib_verifying_store_receipts:resend_receipt_data()
    catch _:R ->
            error_logger:info_msg("resend_receipt_data Fail, Reason ~p, stacktrace  ~p~n", 
                                  [R, erlang:get_stacktrace()]) 
    after
        erlang:send_after(?RESEND_RECEIPT_MILLISECONDS, self(), {resend_receipt_data})
    end,
    {noreply, State};

handle_info({ibrowse_async_headers, _ReqId, _Code, _Headers}, State) ->
    {noreply, State};
handle_info({ibrowse_async_response, ReqId, Response}, State) ->
    Id = get({req_id, ReqId}),
    {Receipt, Callback} = get({verify_receipt, Id}),
    handle_pay_info(Id, Receipt, Callback, Response),
    erase({req_id, ReqId}),
    erase({verify_receipt, Id}),
    {noreply, State};
handle_info({ibrowse_async_response_end, _ReqId}, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    error_logger:info_msg("handle_info Msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_verify_receipt(Url, Id, Receipt, Callback) ->
    {ibrowse_req_id, ReqId} = http:async_request(Url, post, [{<<"receipt-data">>, Receipt}]),
    put({verify_receipt, Id}, {Receipt, Callback}),
    put({req_id, ReqId}, Id).

handle_pay_info(Id, Receipt, Callback, Response) ->
    ReceiptDataList = re:replace(Response, "[\n\t ]", "", [global,caseless,{return, list}]),
    JsonObject = jsx:decode(list_to_binary(ReceiptDataList)),
    error_logger:info_msg("JsonObject: ~p~n", [JsonObject]),
    case lists:keyfind(<<"status">>, 1, JsonObject) of
        {<<"status">>, 0} ->
            error_logger:info_msg("status:0~n"),
            case lists:keyfind(<<"receipt">>, 1, JsonObject) of
                {<<"receipt">>, ProductData} ->                                        
                    case lists:keyfind(<<"product_id">>, 1, ProductData) of
                        {<<"product_id">>, ProductId} ->
                            Quantity = 
                                case lists:keyfind(<<"quantity">>, 1, ProductData) of
                                    false ->
                                        0;
                                    {<<"quantity">>, QuantityBin} ->
                                        binary_to_integer(QuantityBin)
                                end,
                            if 
                                Quantity =< 0 ->
                                    error_logger:info_msg("Invalid Quantity~n", []);
                                true ->
                                    Callback(ProductId)
                            end;
                        false ->
                            error_logger:info_msg("Not Find ProductId In Json Data~n", [])
                    end;                            
                false ->
                    not_receipt
            end;
        {<<"status">>, 21007} ->
            error_logger:info_msg("This receipt is a sandbox receipt, but it was sent to the production service for verification.~n", []),
            do_verify_receipt(?APP_SANDBOX_VERIFY_RECEIPT_URL, Id, Receipt, Callback);
        {<<"status">>, 21008} ->
            error_logger:info_msg("This receipt is a production receipt, but it was sent to the sandbox service for verification.~n", []),
            do_verify_receipt(?APP_VERIFY_RECEIPT_URL, Id, Receipt, Callback);
        {<<"status">>, Other} ->
            error_logger:info_msg("fail, receive status:~p~n", [Other]),                   
            not_status_0;
        false ->                    
            not_status
    end.
