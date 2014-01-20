-module(json).
-export([encode/1, decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% property value can't be atom
encode(Term) ->
    jsx:encode(Term).

decode(Bin) ->
    jsx:decode(Bin).
