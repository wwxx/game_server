-module(game_connection).

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, send_data/2, stop/1, sync_stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(protocol, {ref, socket, transport, playerID, last_active_time}).

-define(ACTIVITY_CHECK_DURATION, 180000).


-include ("include/game_static.hrl").
-include("include/gproc_macros.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    % gen_server:start_link({local, ?MODULE}, ?MODULE, [Ref, Socket, Transport], Opts).
    gen_server:start_link(?MODULE, [Ref, Socket, Transport], Opts).

send_data(Pid, Data) ->
    gen_server:cast(Pid, {send_data, Data}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

sync_stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Ref, Socket, Transport]) ->
    %erlang:start_timer(ACTIVITY_CHECK_DURATION, self(), active_check),
    {ok, #protocol{ref = Ref, socket = Socket, transport = Transport, last_active_time = time_utils:current_time()}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({send_data, Data}, State=#protocol{transport = Transport, socket = Socket}) ->
    io:format("send_data: ~p~n", [Data]),
    Transport:send(Socket, utils_protocol:encode(Data)),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State=#protocol{transport = Transport, socket = Socket}) ->
    ok = ranch:accept_ack(State#protocol.ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),
    {noreply, State};
handle_info({tcp, Socket, RawData}, State=#protocol{transport = Transport}) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    <<RequestType:8/integer, RequestBody/binary>> = RawData,
    error_logger:info_msg("RequestType: ~p, RequestBody: ~p~n", [RequestType, RequestBody]),
    Params = request_decoder:decode(RequestBody, RequestType),
    Path = routes:route(RequestType),
    error_logger:info_msg("Request Path: ~p Parmas: ~p~n", [Path, Params]),
    NewState = handle_request({RequestType, Path, Params}, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_closed, playerID: ~p~n", [State#protocol.playerID]),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Msg}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_error, playerID: ~p~n", [State#protocol.playerID]),
    {stop, normal, State}.

handle_request({RequestType, {sessions_controller, login}, Params},
               State=#protocol{transport=Transport, socket=Socket}) ->
    %{Udid} = utils_protocol:decode(RequestBody, {string}),
    Udid = proplists:get_value(udid, Params),
    io:format("Udid: ~p~n", [Udid]),
    PlayerID = player_data:get_player_id(Udid),
    register_connection(PlayerID),
    %% Start player process
    player_factory:start_player(PlayerID),
    LoginInfo = sessions_controller:login(PlayerID),
    TypeBin = utils_protocol:encode_integer(RequestType),
    ResponseBin = utils_protocol:encode(LoginInfo),
    Transport:send(Socket, list_to_binary([TypeBin, ResponseBin])),
    State#protocol{playerID = PlayerID};
handle_request({RequestType, Path, Params},
               State=#protocol{playerID = PlayerID, transport=Transport, socket=Socket}) ->
    Response = player:request(PlayerID, Path, Params),
    TypeBin = utils_protocol:encode_integer(RequestType),
    ResponseBin = utils_protocol:encode(Response),
    Transport:send(Socket, list_to_binary([TypeBin, ResponseBin])),
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State=#protocol{playerID=PlayerID}) ->
    player_factory:del_con_pid(PlayerID),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_connection(PlayerID) ->
    case ?GET_PID({connection, PlayerID}) of
        OldConPid when OldConPid =/= undefined ->
            game_connection:sync_stop(OldConPid)
    end,
    ?REG_PID({connection, PlayerID}).
