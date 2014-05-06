-module(leaderboard).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {redis}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Default options when creating a leaderboard. Page size is 25 and reverse
    % is set to false, meaning various methods will return results in
    % highest-to-lowest order.
    put(page_size, 25),
    put(reverse, false),
    put(member_key, "member"),
    put(rank_key, "rank"),
    put(score_key, "score"),
    put(member_data_key, "member_data"),
    put(member_data_namespace, "member_data"),

    % Default options when requesting data from a leaderboard.
    % +:with_member_data+ false: Return member data along with the member names.
    % +:page_size+ nil: The default page size will be used.
    % +:members_only+ false: Only return the member name, not their score and rank.
    % +:sort_by+ :none: The default sort for a call to `ranked_in_list`.
    put(with_member_data, false),
    % put(page_size, undefined),
    put(members_only, false),
    put(sort_by, none),

    {ok, Redis} = eredis:start_link(),
    {ok, #state{redis = Redis}}.

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
delete_leaderboard() ->
    LeaderboardName = get(leaderboard_name),
    transaction(fun() ->
        redis_cmd(["DEL", LeaderboardName]),
        redis_cmd(["DEL", member_data_key()])
    end).

% Rank a member in the named leaderboard.
rank_member(Member, Score) ->
    LeaderboardName = get(leaderboard_name),
    redis_cmd(["ZADD", LeaderboardName, Score, Member]).
rank_member(Member, Score, MemberData) ->
    LeaderboardName = get(leaderboard_name),
    transaction(fun() ->
        redis_cmd(["ZADD", LeaderboardName, Score, Member]),
        redis_cmd(["HSET", member_data_key(), Member, MemberData])
    end).


% Rank a member across multiple leaderboards.
rank_member_across(Leaderboards, Member, Score) ->
    transaction(fun() ->
        lists:foreach(fun(Leaderboard) ->
            redis_cmd(["zadd", Leaderboard, Score, Member])
        end, Leaderboards)
    end).
rank_member_across(Leaderboards, Member, Score, MemberData) ->
    transaction(fun() ->
        lists:foreach(fun(Leaderboard) ->
            redis_cmd(["zadd", Leaderboard, Score, Member]),
            redis_cmd(["hset", member_data_key(), Member, MemberData])
        end, Leaderboards)
    end).

member_data_for(Member) ->
    redis_cmd(["hget", member_data_key(), Member]).

update_member_data(Member, MemberData) ->
    redis_cmd(["hset", member_data_key(), Member, MemberData]).

remove_member_data(Member) ->
    redis_cmd(["hdel", member_data_key(), Member]).

rank_members(MembersAndScores) ->
    LeaderboardName = get(leaderboard_name),
    transaction(fun() ->
        lists:foreach(fun({Member, Score}) ->
            redis_cmd(["zadd", LeaderboardName, Score, Member])
        end, MembersAndScores)
    end).

remove_member(Member) ->
    LeaderboardName = get(leaderboard_name),
    transaction(fun() ->
        redis_cmd(["zrem", LeaderboardName, Member]),
        redis_cmd(["hdel", member_data_key(), Member])
    end).

total_members() ->
    redis_cmd(["zcard", get(leaderboard_name)]).

total_pages(page_size = nil) ->
    number:ceil(total_members() / get(page_size)).

total_members_in_score_range(MinScore, MaxScore) ->
    redis_cmd(["zcount", get(leaderboard_name), MinScore, MaxScore]).

change_score_for(Member, Delta) ->
    redis_cmd(["zincrby", get(leaderboard_name), Delta, Member]).

rank_for(Member) ->
    case get(reverse) of
        true ->
            case redis_cmd(["zrank", get(leaderboard_name), Member]) of
                {ok, undefined} -> undefined;
                {ok, Rank} -> binary_to_integer(Rank) + 1
            end;
        false ->
            case redis_cmd(["zrevrank", get(leaderboard_name), Member]) of
                {ok, undefined} -> undefined;
                {ok, Rank} -> binary_to_integer(Rank) + 1
            end
    end.

score_for(Member) ->
    case redis_cmd(["zscore", get(leaderboard_name), Member]) of
        {ok, undefined} -> undefined;
        {ok, Score} -> binary_to_integer(Score)
    end.

is_member_ranked(Member) ->
    case redis_cmd(["zscore", get(leaderboard_name), Member]) of
        {ok, undefined} -> false;
        _ -> true
    end.

score_and_rank_for(Member) ->
    LeaderboardName = get(leaderboard_name),
    Results = transaction(fun() ->
        redis_cmd(["zscore", LeaderboardName, Member]),
        case get(reverse) of
            true -> redis_cmd(["zrank", LeaderboardName, Member]);
            false -> redis_cmd(["zrevrank", LeaderboardName, Member])
        end
    end),

    case Results of
        {ok, [Score, Rank]} when is_binary(Score) andalso is_binary(Rank) ->
            {binary_to_integer(Score), binary_to_integer(Rank) + 1};
        {ok, [Score, Rank]} ->
            {Score, Rank}
    end.

remove_members_in_score_range(MinScore, MaxScore) ->
    redis_cmd(["zremrangebyscore", get(leaderboard_name), MinScore, MaxScore]).

remove_members_outside_rank(Rank) ->
    case get(reverse) of
        true ->
            redis_cmd(["zremrangebyrank", get(leaderboard_name), Rank, -1]);
        false ->
            redis_cmd(["zremrangebyrank", get(leaderboard_name), 0, -(Rank) - 1])
    end.

percentile_for(member) ->
    LeaderboardName = get(leaderboard_name),
    case is_member_ranked(Member) of
        false -> undefined;
        true ->
            transaction(fun() ->
                redis_cmd(["zcard", LeaderboardName]),
                redis_cmd(["zrevrank", LeaderboardName, Member])
            end)
    end.
    responses = @redis_connection.multi do |transaction|
      transaction.zcard(leaderboard_name)
      transaction.zrevrank(leaderboard_name, member)
    end

    percentile = ((responses[0] - responses[1] - 1).to_f / responses[0].to_f * 100).ceil
    if @reverse
      100 - percentile
    else
      percentile
    end
  end

page_for(Member) ->
    LeaderboardName = get(leaderboard_name),
    RankForMember = case get(reverse) of
        true ->
            redis_cmd(["zrank", LeaderboardName, Member]);
        false ->
            redis_cmd(["zrevrank", LeaderboardName, Member])
    end
    Rank = case RankForMember of
        {ok, undefined} -> 0;
        {ok, Rank} -> binary_to_integer(Rank) + 1
    end,
    number:ceil(Rank / get(page_size)).

expire_leaderboard(Seconds) ->
    transaction(fun() ->
        redis_cmd(["expire", get(leaderboard_name), Seconds]),
        redis_cmd(["expire", member_data_key(), Seconds])
    end).

expire_leaderboard_at(Timestamp) ->
    transaction(fun() ->
        redis_cmd(["expireat", get(leaderboard_name), Timestamp]),
        redis_cmd(["expireat", member_data_key(), Timestamp])
    end).

members(current_page, options = {}) ->
    leaderboard_options = DEFAULT_LEADERBOARD_REQUEST_OPTIONS.dup
    leaderboard_options.merge!(options)

    if current_page < 1
      current_page = 1
    end

    page_size = validate_page_size(leaderboard_options[:page_size]) || @page_size

    if current_page > total_pages_in(leaderboard_name, page_size)
      current_page = total_pages_in(leaderboard_name, page_size)
    end

    index_for_redis = current_page - 1

    starting_offset = (index_for_redis * page_size)
    if starting_offset < 0
      starting_offset = 0
    end

    ending_offset = (starting_offset + page_size) - 1

    if @reverse
      raw_leader_data = @redis_connection.zrange(leaderboard_name, starting_offset, ending_offset, :with_scores => false)
    else
      raw_leader_data = @redis_connection.zrevrange(leaderboard_name, starting_offset, ending_offset, :with_scores => false)
    end

    if raw_leader_data
      return ranked_in_list_in(leaderboard_name, raw_leader_data, leaderboard_options)
    else
      return []
    end
  end

transaction(Fun) ->
    Redis = get(redis),
    eredis:q(Redis, ["MULTI"]),
    Fun(),
    eredis:q(Redis, ["EXEC"]).

redis_cmd(List) ->
    Redis = get(redis),
    eredis:q(Redis, List).

member_data_key() ->
    LeaderboardName = get(leaderboard_name),
    LeaderboardName ++ ":" ++ get(member_data_namespace).
