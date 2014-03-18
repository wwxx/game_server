%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2024
%%% Savin Max <mafei.198@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%
%%% @doc
%%%        Chat Channel.
%%% @end
%%% Created :  三  3 12 17:29:50 2014 by Savin Max

-module(chat_channel).

-behaviour(gen_server).

%% API
-export([start_link/3,
         history/1,
         broadcast/2,
         join/2,
         leave/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/gproc_macros.hrl").

-record(state, {channel, maxCacheTime, maxCacheAmount}).

-define(TAB, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, MaxCacheTime, MaxCacheAmount) ->
    gen_server:start_link(?MODULE, [Id, MaxCacheTime, MaxCacheAmount], []).

join(PlayerID, Channel) ->
    player:subscribe(PlayerID, Channel).

leave(PlayerID, Channel) ->
    player:unsubscribe(PlayerID, Channel).

% cached history messages.
history(_Channel) ->
    ok.

broadcast(Channel, Msg) ->
    Pid = case ?GET_PID({chat_channel, Channel}) of
              undefined ->
                  chat_server:create_channel(Channel);
              ChannelPid ->
                  ChannelPid
          end,
    gen_server:cast(Pid, {broadcast, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Channel, MaxCacheTime, MaxCacheAmount]) ->
    ?REG_PID({chat_channel, Channel}),
    {ok, #state{channel=Channel, maxCacheTime=MaxCacheTime, maxCacheAmount = MaxCacheAmount}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({broadcast, Msg}, State=#state{channel=Channel}) ->
    ?PUBLISH(Channel, {gproc_msg, Channel, Msg}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel=Channel}) ->
    ?UNREG({chat_channel, Channel}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
