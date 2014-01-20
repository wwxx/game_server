%%% $Id: erlang.snippets,v 1.1 2011/02/19 16:36:37 micro Exp $
%%% @author  Savin-Max mafei.198@gmail.com
%%% @copyright (C) © 2013, Savin-Max. All Rights Reserved.
%%% @doc
%%%        Some easy to use functions for operate ets.
%%% @end
%%% Created :  一 10 07 00:01:33 2013 by Savin-Max µ

-module(ets_utils).

-export([makepat/1,
         make_element_spec/1
        ]).

%%--------------------------------------------------------------------
%% @doc:    Construct {'_', Value1, ..., '_', ValueN} for ets:match_object(Tab, Pattern)
%% @spec:    makepat(SelectorRecord::record() ) -> tuple().
%% @end
%%--------------------------------------------------------------------

-spec(makepat(record()) -> tuple() ).
makepat(Record) ->
    makepat(tuple_to_list(Record), []).

makepat([], Result) ->
    list_to_tuple(Result);
makepat([Value|ValueList], Result) when Value =:= undefined ->
    makepat(ValueList, Result ++ ['_']);
makepat([Value|ValueList], Result) ->
    makepat(ValueList, Result ++ [Value]).

%%--------------------------------------------------------------------
%% @doc:    Construct [{Pos, Value}, ...] for ets:update_element(Tab, Key, [{Pos, Value}])
%% @spec:    make_element_spec(ModifierRecord::record( ) -> [tuple(), ...].
%% @end
%%--------------------------------------------------------------------

-spec(make_element_spec(record()) -> [tuple(), ...]).
make_element_spec(ModifierRecord) ->
    [_Name|ValueList] = tuple_to_list(ModifierRecord),
    make_element_spec(ValueList, 2, []).

make_element_spec([], _Position, Result) ->
    Result;
make_element_spec([Value|ValueList], Position, Result) when Value =:= undefined ->
    make_element_spec(ValueList, Position + 1, Result);
make_element_spec([Value|ValueList], Position, Result) ->
    make_element_spec(ValueList, Position + 1, Result ++ [{Position, Value}]).
