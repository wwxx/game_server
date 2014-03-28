-define(FALSE, 0).
-define(TRUE, 1).

-define(OK, 200).
-define(ERROR, 298).

-define(MODEL_ORIGIN, 1).
-define(MODEL_UPDATE, 2).
-define(MODEL_DELETE, 3).
-define(MODEL_CREATE, 4).

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

