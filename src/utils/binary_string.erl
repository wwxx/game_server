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
%%%        Binary String utils.
%%% @end
%%% Created :  二  3 11 14:01:02 2014 by Savin Max

-module(binary_string).

-export([split/2, join/2, is_valid_for_mysql/1, clean_for_mysql/1]).

split(BinaryString, Separator) ->
    split(BinaryString, Separator, []).

split(BinaryString, Separator, Result) ->
    case binary:split(BinaryString, Separator) of
        [BinaryString] ->
            lists:reverse([BinaryString|Result]);
        [Head, RemainBinaryString] ->
            split(RemainBinaryString, Separator, [Head|Result])
    end.

join(BinaryStringList, Separator) ->
    join(BinaryStringList, Separator, <<>>).

%%% Private Methods
join([], _Separator, Result) ->
    Result;
join([BinaryString|BinaryStringList], Separator, <<>>) ->
    join(BinaryStringList, Separator, BinaryString);
join([BinaryString|BinaryStringList], Separator, Result) ->
    join(BinaryStringList, Separator, <<Result/binary, Separator/binary, BinaryString/binary>>).


-define(INVALID_MYSQL_UTF8_RE, "[\x{010000}-\x{10FFFF}]").

is_valid_for_mysql(BinString) ->
    case re:run(unicode:characters_to_list(BinString), ?INVALID_MYSQL_UTF8_RE, [unicode]) of
        nomatch -> true;
        _ -> false
    end.

clean_for_mysql(BinString) ->
    Res = re:replace(unicode:characters_to_list(BinString), ?INVALID_MYSQL_UTF8_RE, "", [unicode]),
    list_to_binary(Res).
