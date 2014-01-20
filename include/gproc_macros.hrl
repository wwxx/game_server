% Global gproc
%-define(GPROC_KEY(Name), {n, g, Name}).
%-define(GET_PID(Name), gproc:lookup_global_name(Name)).
%-define(REG_PID(Name), gproc:add_global_name(Name)). %% Only can reg yourself.
%-define(UNREG(Name), gproc:unreg(?GPROC_KEY(Name))).

% local gproc
-define(GPROC_KEY(Name), {n, l, Name}).
-define(GET_PID(Name), gproc:where(?GPROC_KEY(Name))).
-define(REG_PID(Name), gproc:reg(?GPROC_KEY(Name))). %% Only can reg yourself.
-define(UNREG(Name), gproc:unreg(?GPROC_KEY(Name))).

