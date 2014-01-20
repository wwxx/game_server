-module (game_env).

-behaviour (gen_server).

-export ([start_link/0,
          exists/1,
          get/1,
          set/2,
          del/1]).

-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record(env_state, {}).

-define (TAB, ?MODULE).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

exists(Key) ->
    ets:member(?TAB, Key).

get(Key) ->
    case ets:lookup(?TAB, Key) of
        [{Key, Val}] ->
            Val;
        _ ->
            undefined
    end.

set(Key, Value) ->
    ets:insert(?TAB, {Key, Value}).

del(Key) ->
    ets:delete(?TAB, Key).

%% gen_server callbacks
init([]) ->
    {ok, #env_state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
