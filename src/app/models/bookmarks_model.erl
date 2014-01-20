-module(bookmarks_model).

-export([load_data/1]).

-include("include/db_schema.hrl").

load_data(PlayerID) ->
    {ok, User} = db:find_one(#users{'_id' = PlayerID}, #users{bookmarks = 1}),
    if
        User#users.bookmarks =:= undefined ->
            {ok, []};
        true ->
            Bookmarks = lists:map(fun(Bookmark) ->
                Proplist = [user_id, PlayerID|tuple_to_list(Bookmark)],
                Document = list_to_tuple(Proplist),
                record_mapper:unmap(bookmarks, Document)
            end, User#users.bookmarks),
            {ok, Bookmarks}
    end.
