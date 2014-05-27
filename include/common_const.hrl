-define(FALSE, 0).
-define(TRUE, 1).

-define(OK, 200).
-define(ERROR, 298).

-define(LETTER_PERSONAL, 1).

-record(letter, {id,
                 category,
                 subject,
                 content,
                 sender_id,
                 sender_name,
                 receiver_id,
                 sent_at,
                 is_read,
                 is_claimed,
                 attachment}).

-define(assert_error_msg(Response, ErrorAtom), 
        case error_msg:const(ErrorAtom) of
        {fail, ErrorAtom} ->
            ?_assertEqual([{code, 0}, {desc, atom_to_binary(ErrorAtom, utf8)}], Response);
        ErrorCode ->
            ?_assertEqual(ErrorCode, proplists:get_value(code, Response))
        end
       ).

-define(assert_no_error_msg(Response), ?_assertNotMatch([{code, _}, {desc, _}], Response)).

