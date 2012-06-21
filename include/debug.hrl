%% Provide simple debugging mechanism

-ifndef(DEBUG_HRL).
-define(DEBUG_HRL, 1).

-define(DEBUG_ENABLE, true).

-ifdef(DEBUG_ENABLE).
-define(dbg(X,Y), io:format("*dbg ~p(~p): " X, [?MODULE, ?LINE | Y])).
-define(dbg(X), io:format(X)).
-else.
-define(dbg(X,Y), ok).
-define(dbg(X), ok).
-endif.

-endif.
