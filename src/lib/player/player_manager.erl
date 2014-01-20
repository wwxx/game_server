-module(player_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_player/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define (TAB, ?MODULE).

-include("include/gproc_macros.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(start_player(PlayerID::binary()) -> {ok, pid()}).
start_player(PlayerID) ->
    gen_server:call(?MODULE, {start_player, PlayerID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({start_player, PlayerID}, _From, State) ->
    Result = case ?GET_PID({player, PlayerID}) of
        undefined ->
            player_sup:start_child([PlayerID]);
        Pid ->
            {ok, Pid}
    end,
    {reply, Result, State}.

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
