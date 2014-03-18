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


%%% @doc
%%%        Some easy to use functions for operating models
%%% @end
%%% Created :  一 10 07 20:08:42 2013 by Savin-Max µ

-module(model_utils).

-compile(export_all).

%%--------------------------------------------------------------------
%% @doc:    Merge Binary String ID and ExtraValue to record tuple
%% @spec:    merge(Record::tuple()) -> tuple().
%% @end
%%--------------------------------------------------------------------

-spec(info(record()) -> tuple()).
info(Record) ->
    [_Name, Key|Values] = tuple_to_list(Record),
    Id = db:objectid_to_binary_string(Key),
    list_to_tuple([Id|Values]).

-spec(info(record(), list()) -> tuple()).
info(Record, ExtraValueList) ->
    [_Name, Key|Values] = tuple_to_list(Record),
    Id = db:objectid_to_binary_string(Key),
    L = lists:append([Id|Values], ExtraValueList),
    list_to_tuple(L).
