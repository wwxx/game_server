%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
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
