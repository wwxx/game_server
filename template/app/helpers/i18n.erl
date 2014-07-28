-module(i18n).
-export([t/3, t/4]).

t(ConfName, Key, Locale0) ->
    Locale = if
                 Locale0 =:= undefined -> en;
                 true -> binary_to_atom(Locale0, utf8)
             end,
    Conf = game_numerical:find(ConfName, Key),
    record_mapper:get_field(Conf, Locale).

t(ConfName, Key, Locale0, Values) ->
    Locale = if
                 Locale0 =:= undefined -> en;
                 true -> binary_to_atom(Locale0, utf8)
             end,
    Conf = game_numerical:find(ConfName, Key),
    % error_logger:info_msg("ALL: ~p, ConfName: ~p, Key: ~p~n", 
    %                       [game_numerical:all(ConfName), ConfName, Key]),
    FormaterString = record_mapper:get_field(Conf, Locale),
    Result = io_lib:format(binary_to_list(FormaterString), Values),
    list_to_binary(Result).
