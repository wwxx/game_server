-module(name_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([new_user_name/0,
         change_user_name/2,
         delete_user_name/1]).

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

new_user_name() ->
    gen_server:call(?SERVER, new_user_name).

change_user_name(OldName, NewName) ->
    gen_server:call(?SERVER, {change_user_name, OldName, NewName}).

delete_user_name(Name) ->
    gen_server:cast(?SERVER, {delete_user_name, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(new_user_name, _From, State) ->
    UserName = gen_user_name(),
    {reply, UserName, State};
handle_call({change_user_name, OldName, NewName}, _From, State) ->
    Success = case game_counter:get({user_name, NewName}) of
                  undefined -> 
                      game_counter:set({user_name, NewName}, true),
                      game_counter:del({user_name, OldName}),
                      true;
                  _ -> false
              end,
    {reply, Success, State}.

handle_cast({delete_user_name, Name}, State) ->
    game_counter:del({user_name, Name}),
    {noreply, State};
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

gen_user_name() ->
    UserName = list_to_binary(["Guest:", game_counter:incr(guest_user_counter)]),
    case game_counter:get({user_name, UserName}) of
        undefined -> 
            game_counter:set({user_name, UserName}, true),
            UserName;
        _ -> gen_user_name()
    end.
