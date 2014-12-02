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
%%%       Manage player's ets data:
%%% @end
%%% Created :  日 10 06 23:34:35 2013 by Savin-Max µ

-module(player_data).

%% API
-export([get_player_id/1,
         create/2,
         delete/2,
         update/2,
         update/3,
         find/2,
         where/2,
         all/2,
         count/2,
         count_all/2,

         get_all_recs/2,
         ensure_load_data/2,
         update_rec_status/4,
         delete_rec_status/4,
         update_rec/2,
         delete_rec/3,
         create_rec/2
        ]).

-include("include/common_const.hrl").
-define(DATA_STATUS, player_ets_data_status).
-define(DATA_LOADED, player_ets_data_loaded).

%%%===================================================================
%%% API
%%%===================================================================

%% Only can be invoked by game_connection
get_player_id(Udid) ->
    PlayerID = users_model:get_player_id(Udid),
    case application:get_env(game_server, server_environment) of
        {ok, test} -> player:proxy(PlayerID, model, persist_all, []);
        _ -> do_nothing
    end,
    PlayerID.

create(PlayerID, Record) when is_tuple(Record) ->
    case validate_ownership(PlayerID) of
        true -> model:create(Record);
        false -> player:proxy(PlayerID, model, create, [Record])
    end;
create(PlayerID, Records) when is_list(Records) ->
    case validate_ownership(PlayerID) of
        true ->
            lists:foreach(fun(Record) ->
                              model:create(Record)
                          end, Records);
        false ->
            lists:foreach(fun(Record) ->
                              player:proxy(PlayerID, model, create, [Record])
                          end, Records)
    end.

delete(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:delete(Selector);
        false -> player:proxy(PlayerID, model, delete, [Selector])
    end.

update(PlayerID, NewRecord) ->
    case validate_ownership(PlayerID) of
        true -> model:update(NewRecord);
        false -> player:proxy(PlayerID, model, update, [NewRecord])
    end.

update(PlayerID, Selector, Modifier) ->
    case validate_ownership(PlayerID) of
        true -> model:update(Selector, Modifier);
        false -> player:proxy(PlayerID, model, update, [Selector, Modifier])
    end.

find(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:find(Selector);
        false -> player:proxy(PlayerID, model, find, [Selector])
    end.

where(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:where(Selector);
        false -> player:proxy(PlayerID, model, where, [Selector])
    end.

all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:all(Table);
        false -> player:proxy(PlayerID, model, all, [Table])
    end.

count(PlayerID, Selector) ->
    case validate_ownership(PlayerID) of
        true -> model:count(Selector);
        false -> player:proxy(PlayerID, model, count, [Selector])
    end.

count_all(PlayerID, Table) ->
    case validate_ownership(PlayerID) of
        true -> model:count_all(Table);
        false -> player:proxy(PlayerID, model, count_all, [Table])
    end.

ensure_load_data(PlayerID, Table) ->
    EtsTab = ets_tab_name(Table),
    case get_loaded(PlayerID, Table) of
        true  -> true;
        false ->
            PlayerID = get(player_id),
            Module = list_to_atom(atom_to_list(Table) ++ "_model"),
            case Module:load_data(PlayerID) of
                {ok, []} -> undefined;
                {ok, Recs} -> insert_recs(PlayerID, Recs, Module, EtsTab)
            end,
            set_loaded(PlayerID, Table, true)
    end.

get_all_recs(PlayerID, Table) ->
    Key = {PlayerID, Table},
    Tab = ets_tab_name(Table),
    lists:foldl(fun({_Key, Id}, Result) ->
        case ets:lookup(Tab, Id) of
            [] -> Result;
            [Rec] -> [Rec|Result]
        end
    end, [], ets:lookup(?DATA_STATUS, Key)).

update_rec_status(PlayerID, Table, Id, Status) ->
    ets:insert(?DATA_STATUS, {PlayerID, {Table, Id, Status}}).

delete_rec_status(PlayerID, Table, Id, Status) ->
    ets:delete_object(?DATA_STATUS, {PlayerID, {Table, Id, Status}}).

update_rec(Table, Rec) ->
    EtsTab = ets_tab_name(Table),
    ets:insert(EtsTab, Rec).

delete_rec(PlayerID, Table, Id) ->
    EtsTab = ets_tab_name(Table),
    ets:insert(?DATA_STATUS, {{PlayerID, Table}, Id}),
    ets:insert(EtsTab, Id).

create_rec(Table, Rec) ->
    update_rec(Table, Rec).

%%%===================================================================
%%% Internal functions
%%%===================================================================
validate_ownership(PlayerID) ->
    PlayerID =:= get(player_id).

get_loaded(PlayerID, ModelName) ->
    Key = {PlayerID, ModelName, loaded},
    case ets:lookup(?DATA_LOADED, Key) of
        [{Key, Value}] ->
            Value;
        _ ->
            undefined
    end.

set_loaded(PlayerID, ModelName, Loaded) ->
    Key = {PlayerID, ModelName, loaded},
    if
        Loaded =:= true ->
            ets:insert(?DATA_LOADED, {Key, true});
        true ->
            ets:delete(?DATA_LOADED, Key)
    end.

insert_recs(PlayerID, Recs, Module, EtsTab) ->
    case lists:keyfind(serialize, 1, Module:module_info(attributes)) of
        false ->
            lists:foreach(fun(Rec) -> insert_rec(PlayerID, Rec, EtsTab) end, Recs);
        {serialize, Rule} ->
            Fields = record_mapper:get_mapping(hd(tuple_to_list(hd(Recs)))),
            lists:foreach(fun(Rec) -> 
                NewRec = deserialize(Rec, Fields, Rule),
                insert_rec(PlayerID, NewRec, EtsTab)
            end, Recs)
    end.

insert_rec(PlayerID, Rec, EtsTab) ->
    ets:insert(EtsTab, Rec),
    [Table, Id|_] = tuple_to_list(Rec),
    ets:insert(?DATA_STATUS, {{PlayerID, Table}, Id}),
    update_rec_status(PlayerID, Table, Id, ?MODEL_ORIGIN).

deserialize(Rec, Fields, Rule) ->
    [RecName|Values] = tuple_to_list(Rec),
    TermValues = deserialize(Values, Fields, Rule, []),
    list_to_tuple([RecName|TermValues]).

deserialize([], [], _Rule, Result) -> 
    lists:reverse(Result);
deserialize([Value|Values], [Field|Fields], Rule, Result) ->
    case lists:member(Field, Rule) of
        true when Value =/= undefined -> 
            TermValue = case base64:decode(Value) of
                <<>> -> undefined;
                Data -> binary_to_term(Data)
            end,
            deserialize(Values, Fields, Rule, [TermValue|Result]);
        _ ->
            deserialize(Values, Fields, Rule, [Value|Result])
    end.

ets_tab_name(Table) ->
    list_to_atom(atom_to_list(Table) ++ "_ets_table").
