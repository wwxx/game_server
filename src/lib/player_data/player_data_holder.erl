%%%-------------------------------------------------------------------
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
%%
%%% @doc
%%%        Hold ets tables
%%% @end
%%% Created :  五 10 11 00:27:26 2013 by Savin-Max
%%%-------------------------------------------------------------------
-module(player_data_holder).

-behaviour(gen_server).

%% API
-export([start_link/0, table/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

table(ModelName) ->
    Name = list_to_atom("model_" ++ atom_to_list(ModelName)),
    case ets:info(Name) of
        undefined ->
            Name = gen_server:call(?MODULE, {create_table, Name});
        _ ->
            Name
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    player_data_state_tracker = ets:new(player_data_state_tracker,
        [set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, #state{}}.

handle_call({create_table, Name}, _From, State) ->
    case ets:info(Name) of
        undefined ->
            Name = ets:new(Name, ets_opts()),
            Name;
        _ ->
            Name
    end,
    {reply, Name, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ets_opts() ->
    [set, public, named_table, {keypos, 2},
     {read_concurrency, true}, {write_concurrency, true}].
