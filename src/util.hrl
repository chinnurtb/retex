-define(RETRIES, 10). % completely arbitrary choice
-define(TRANS(F), mnesia:transaction(fun () -> F end, ?RETRIES)).
