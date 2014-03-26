%% The MIT License (MIT)
%%
%% Copyright (c) 2014-2024
%% Savin Max <mafei.198@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.


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


-include("include/gproc_macros.hrl").
-include("../app/include/secure.hrl").
-include ("include/common_const.hrl").

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
    error_logger:info_msg("send_data: ~p~n", [Data]),
    send_socket_data(Transport, Socket, encode_response(Data)),
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
    ok = Transport:setopts(Socket, [{active, once}, {packet, 2}]),
    {noreply, State};
handle_info({tcp, Socket, CipherData}, State=#protocol{transport = Transport}) ->
    error_logger:info_msg("CipherData: ~p~n", [CipherData]),
    ok = Transport:setopts(Socket, [{active, once}]),
    RawData = secure:decrypt(?AES_KEY, ?AES_IVEC, CipherData),
    {RequestType, RequestBody} = utils_protocol:decode_short(RawData),
    error_logger:info_msg("RequestType: ~p, RequestBody: ~p~n", [RequestType, RequestBody]),
    Path = case routes:route(RequestType) of
               {error, Msg} ->
                   Response = api_encoder:encode({fail, {0, Msg}}),
                   send_socket_data(Transport, Socket, Response);
               RoutePath ->
                   RoutePath
           end,
    {Params, _LeftData} = api_decoder:decode(RawData),
    error_logger:info_msg("Request Path: ~p Parmas: ~p~n", [Path, Params]),
    handle_request({Path, Params}, State);
handle_info({tcp_closed, _Socket}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_closed, playerID: ~p~n", [State#protocol.playerID]),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _Msg}, State) ->
    error_logger:info_msg("DISCONNECT: tcp_error, playerID: ~p~n", [State#protocol.playerID]),
    {stop, normal, State};
handle_info(Msg, State) ->
    error_logger:info_msg("unhandled Msg: ~p~n", Msg),
    {noreply, State}.

handle_request({{sessions_controller, login}, Params},
               State=#protocol{transport=Transport, socket=Socket}) ->
    %{Udid} = utils_protocol:decode(RequestBody, {string}),
    Udid = proplists:get_value(udid, Params),
    error_logger:info_msg("Udid: ~p~n", [Udid]),
    PlayerID = player_data:get_player_id(Udid),
    register_connection(PlayerID),
    %% Start player process
    player_factory:start_player(PlayerID),
    LoginInfo = sessions_controller:login(PlayerID),
    send_socket_data(Transport, Socket, encode_response(LoginInfo)),
    {noreply, State#protocol{playerID = PlayerID}};
handle_request({Path, Params}, State=#protocol{playerID = PlayerID, transport=Transport, socket=Socket}) ->
    case PlayerID of
        undefined ->
            {stop, {playerID, undefined}, State};
        _ ->
            Response = player:request(PlayerID, Path, proplists_utils:values(Params)),
            error_logger:info_msg("Path: ~p, Params: ~p, Response: ~p~n", [Path, Params, Response]),
            send_socket_data(Transport, Socket, encode_response(Response)),
            {noreply, State}
    end.

encode_response(Response) ->
    {Protocol, Msg} = Response,
    case Response of
        {fail, ErrorAtom} ->
            case error_msg:const(ErrorAtom) of
                {fail, ErrorAtom} ->
                    api_encoder:encode({fail, {0, atom_to_list(ErrorAtom)}});
                ErrorCode ->
                    api_encoder:encode({fail, {ErrorCode, <<"">>}})
            end;
        {fail, ErrorAtom, Msg} ->
            ErrorCode = error_msg:const(ErrorAtom),
            case error_msg:const(ErrorAtom) of
                {fail, ErrorAtom} ->
                    api_encoder:encode({fail, {0, atom_to_list(ErrorAtom)}});
                ErrorCode ->
                    api_encoder:encode({fail, {ErrorCode, Msg}})
            end;
        {Protocol, Msg} when is_tuple(Msg) ->
            api_encoder:encode(Protocol, Msg);
        {Protocol, Msg} when is_list(Msg) ->
            api_encoder:encode(Protocol, {Msg});
        {Protocol, Msg} ->
            erlang:error(io_lib:format("Response Msg type error: ~p", [Msg])),
            api_encoder:encode(ok, {?OK})
    end.

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
    case PlayerID =:= undefined of
        false -> ?UNREG({connection, PlayerID});
        true -> ok
    end,
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
        undefined -> ok;
        OldConPid -> game_connection:sync_stop(OldConPid)
    end,
    ?REG_PID({connection, PlayerID}).

send_socket_data(Transport, Socket, Data) ->
    CipherData = secure:encrypt(?AES_KEY, ?AES_IVEC, Data),
    Transport:send(Socket, CipherData).
