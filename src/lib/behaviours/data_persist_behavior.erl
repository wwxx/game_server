-module(data_persist_behavior).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [   %% -spec(persist(ModelName::atom(),
        %%              Id::any(),
        %%              Status::delete|update|create,
        %%              Value::undefined|[ChangedField::atom(), ...]) -> ok).
        {persist, 4}
    ];
behaviour_info(_) ->
    undefined.
