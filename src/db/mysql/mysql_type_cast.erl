-module(mysql_type_cast).

-export([cast/2]).

cast(Type, Value) ->
    case Value of
        null -> undefined;
        Value -> 
            Fun = cast_fun_for(Type),
            Fun(Value)
    end.

cast_fun_for(Type) ->
    Map = [{'VAR_STRING', fun identity/1},
           {'TINYBLOB', fun identity/1},
           {'MEDIUM_BLOG', fun identity/1},
           {'LONG_BLOG', fun identity/1},
           {'BLOB', fun identity/1},
           {'VAR_STRING', fun identity/1},
           {'TINY', fun to_integer/1},
           {'SHORT', fun to_integer/1},
           {'LONG', fun to_integer/1},
           {'LONGLONG', fun to_integer/1},
           {'INT24', fun to_integer/1},
           {'YEAR', fun to_integer/1},
           {'DECIMAL', fun to_float/1},
           {'FLOAT', fun to_float/1},
           {'DOUBLE', fun to_float/1},
           {'DATE', fun to_date/1},
           {'TYPE', fun to_time/1},
           {'TIMESTAMP', fun to_timestamp/1},
           {'DATETIME', fun to_timestamp/1},
           {'BIT', fun to_bit/1}],
    case lists:keyfind(Type, 1, Map) of
        false -> fun identity/1;
        {Type, F} -> F
    end.

identity(String) -> list_to_binary(String).
to_integer(String) -> list_to_integer(String).
to_float(String) ->
    {ok, [Num], _Leftovers} = case io_lib:fread("~f", String) of
        {error, _} ->
          case io_lib:fread("~d", String) of
            {ok, [_], []} = Res ->
              Res;
            {ok, [X], E} ->
              io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s" ,[X,".0",E])))
          end;
        Res -> Res
    end,
    Num.
to_date(String) ->
    case io_lib:fread("~d-~d-~d", String) of
        {ok, [Year, Month, Day], _} -> {date, {Year, Month, Day}};
        {error, _} -> String;
        _ -> exit({error, bad_date})
    end.
to_time(String) ->
    case io_lib:fread("~d:~d:~d", String) of
        {ok, [Hour, Minute, Second], _} -> {time, {Hour, Minute, Second}};
        {error, _} -> String;
        _ -> exit({error, bad_time})
    end.
to_timestamp(String) ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", String) of
        {ok, [Year, Month, Day, Hour, Minute, Second], _} ->
            {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
        {error, _} -> String;
        _ -> exit({error, datetime})
    end.
to_bit(<<1>>) -> 1;
to_bit(<<0>>) -> 0.
