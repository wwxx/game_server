-module(leaderboard).

-behaviour(gen_server).

%% API
-export([start_link/1, proxy/2, proxy/3, wrap/2]).

%% Warning: Invoke from proxy!!!
-export([delete_leaderboard/0,
         rank_member/2,
         rank_member/3,
         member_data_for/1,
         update_member_data/2,
         remove_member_data/1,
         rank_members/1,
         remove_member/1,
         total_members/0,
         total_pages/0,
         total_pages/1,
         total_members_in_score_range/2,
         change_score_for/2,
         rank_for/1,
         score_for/1,
         is_member_ranked/1,
         score_and_rank_for/1,
         remove_members_in_score_range/2,
         remove_members_outside_rank/1,
         page_for/1,
         expire_leaderboard/1,
         expire_leaderboard_at/1,
         members/1,
         all_members/0,
         members_from_score_range/2,
         members_from_rank_range/2,
         top_member/0,
         member_at/1,
         around_me/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("include/gproc_macros.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(LeaderboardName) ->
    gen_server:start_link(?MODULE, [LeaderboardName], []).

proxy(LeaderboardName, Fun) ->
    gen_server:call(leaderboard_pid(LeaderboardName), {proxy, Fun, []}).

proxy(LeaderboardName, Fun, Args) ->
    gen_server:call(leaderboard_pid(LeaderboardName), {proxy, Fun, Args}).

wrap(LeaderboardName, Fun) ->
    gen_server:call(leaderboard_pid(LeaderboardName), {wrap, Fun}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LeaderboardName]) ->
    ?REG_PID({leaderboard, LeaderboardName}),
    put(leaderboard_name, LeaderboardName),
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
    put(with_member_data, true),
    % put(page_size, undefined),
    put(members_only, false),
    put(sort_by, none),

    {ok, Redis} = eredis:start_link(),
    put(redis, Redis),
    {ok, #state{}}.

handle_call({proxy, Fun, Args}, _From, State) ->
    Reply = apply(?MODULE, Fun, Args),
    {reply, Reply, State};
handle_call({wrap, Fun}, _From, State) ->
    {reply, Fun(), State};
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
    redis_cmd(["zadd", LeaderboardName, Score, Member]).
rank_member(Member, Score, MemberData) ->
    LeaderboardName = get(leaderboard_name),
    transaction(fun() ->
        redis_cmd(["zadd", LeaderboardName, Score, Member]),
        redis_cmd(["hset", member_data_key(), Member, MemberData])
    end).

member_data_for(Member) ->
    case redis_cmd(["hget", member_data_key(), Member]) of
        {ok, undefined} -> undefined;
        {ok, Data} -> decode_member_data(Data)
    end.

update_member_data(Member, MemberData) ->
    redis_cmd(["hset", member_data_key(), Member, encode_member_data(MemberData)]).

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
    {ok, AmountBin} = redis_cmd(["zcard", get(leaderboard_name)]),
    binary_to_integer(AmountBin).

total_pages() ->
    total_pages(get(page_size)).
total_pages(PageSize) ->
    number:ceil(total_members() / PageSize).

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

page_for(Member) ->
    LeaderboardName = get(leaderboard_name),
    RankForMember = case get(reverse) of
        true ->
            redis_cmd(["zrank", LeaderboardName, Member]);
        false ->
            redis_cmd(["zrevrank", LeaderboardName, Member])
    end,
    Rank = case RankForMember of
        {ok, undefined} -> 0;
        {ok, RankBin} -> binary_to_integer(RankBin) + 1
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

members(CurrentPage0) ->
    members(CurrentPage0, []).

members(CurrentPage0, Options) ->
    PageSize = case proplists:get_value(page_size, Options) of
                   undefined -> get(page_size);
                   Size -> Size
               end,
    TotalPages = total_pages(PageSize),
    CurrentPage = lists:min([lists:max([CurrentPage0, 1]), TotalPages]),
    IndexForRedis = CurrentPage - 1,
    StartingOffset = lists:max([IndexForRedis * PageSize, 0]),
    EndingOffset = StartingOffset + PageSize - 1,

    RawLeaderData = case get(reverse) of
        true ->
            redis_cmd(["zrange", get(leaderboard_name), StartingOffset, EndingOffset]);
        false ->
            redis_cmd(["zrevrange", get(leaderboard_name), StartingOffset, EndingOffset])
    end,

    case RawLeaderData of
        {ok, undefined} -> [];
        {ok, Data} -> ranked_in_list(Data)
    end.

all_members() ->
    RawLeaderData = case get(reverse) of
        true ->
            redis_cmd(["zrange", get(leaderboard_name), 0, -1]);
        false ->
            redis_cmd(["zrevrange", get(leaderboard_name), 0, -1])
    end,

    case RawLeaderData of 
        {ok, undefined} -> [];
        {ok, Data} -> ranked_in_list(Data)
    end.

members_from_score_range(MinimumScore, MaximumScore) ->
    RawLeaderData = case get(reverse) of
        true ->
            redis_cmd(["zrangebyscore", get(leaderboard_name), MinimumScore, MaximumScore]);
        false ->
            redis_cmd(["zrevrangebyscore", get(leaderboard_name), MaximumScore, MinimumScore])
    end,

    case RawLeaderData of
        {ok, undefined} -> [];
        {ok, Data} -> ranked_in_list(Data)
    end.

members_from_rank_range(StartingRank0, EndingRank0) ->
    StartingRank = lists:max([StartingRank0 - 1, 0]),

    TotalMembers = total_members(),
    EndingRank = lists:min([EndingRank0 - 1, TotalMembers - 1]),

    RawLeaderData = case get(reverse) of
        true ->
            redis_cmd(["zrange", get(leaderboard_name), StartingRank, EndingRank]);
        false ->
            redis_cmd(["zrevrange", get(leaderboard_name), StartingRank, EndingRank])
    end,

    case RawLeaderData of
        {ok, undefined} -> [];
        {ok, Data} -> ranked_in_list(Data)
    end.

top_member() ->
    case members_from_rank_range(1, 1) of
        [] -> undefined;
        [Member] -> Member
    end.

member_at(Position) ->
    member_at(Position, []).
member_at(Position, Options) ->
    case Position =< total_members() of
        true ->
            PageSize = case proplists:get_value(page_size, Options) of
                           undefined -> get(page_size);
                           Size -> Size
                       end,
            CurrentPage = number:ceil(Position/ PageSize),
            case members(CurrentPage, Options) of
                [] -> undefined;
                Leaders -> lists:nth(Position, Leaders)
            end;
        false -> undefined
    end.

around_me(Member) ->
    around_me(Member, []).
around_me(Member, Options) ->
    ReverseRankForMember = case get(reverse) of
        true ->
            redis_cmd(["zrank", get(leaderboard_name), Member]);
        false ->
            redis_cmd(["zrevrank", get(leaderboard_name), Member])
    end,

    case ReverseRankForMember of 
        {ok, undefined} -> [];
        {ok, RankBin} ->
            Rank = binary_to_integer(RankBin),
            PageSize = case proplists:get_value(page_size, Options) of
                           undefined -> get(page_size);
                           Size -> Size
                       end,
            StartingOffset0 = Rank - trunc(PageSize / 2),
            StartingOffset = lists:max([StartingOffset0, 0]),
            EndingOffset = StartingOffset + PageSize - 1,
            RawLeaderData = case get(reverse) of
                true ->
                    redis_cmd(["zrange", get(leaderboard_name), StartingOffset, EndingOffset]);
                false ->
                    redis_cmd(["zrevrange", get(leaderboard_name), StartingOffset, EndingOffset])
            end,
            case RawLeaderData of 
                {ok, undefined} -> [];
                {ok, Data} -> ranked_in_list(Data)
            end
    end.

ranked_in_list(Members) ->
    {ok, RanksAndScores} = transaction(fun() ->
        lists:foreach(fun(Member) ->
            case get(reverse) of
                true -> redis_cmd(["zrank", get(leaderboard_name), Member]);
                false -> redis_cmd(["zrevrank", get(leaderboard_name), Member])
            end,
            redis_cmd(["zscore", get(leaderboard_name), Member])
        end, Members)
    end),
    ranked_in_list(Members, RanksAndScores, []).

ranked_in_list([], [], Result) -> lists:reverse(Result);
ranked_in_list([Member|Members], [Rank, Score|RanksAndScores], Result) ->
    ranked_in_list(Members, RanksAndScores, [{Member, binary_to_integer(Rank) + 1, binary_to_integer(Score) + 1, member_data_for(Member)}|Result]).


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

leaderboard_pid(LeaderboardName) ->
    ?GET_PID({leaderboard, LeaderboardName}).

encode_member_data(Data) ->
    Value = term_to_binary(Data),
    base64:encode(Value).

decode_member_data(Value) ->
    Data = base64:decode(Value),
    binary_to_term(Data).
