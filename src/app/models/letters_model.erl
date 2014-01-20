-module(letters_model).

-export([unread_letter_counts/1]).

unread_letter_counts(_PlayerID) ->
    [{personal, 1}].
