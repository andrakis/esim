%% Test header file. Include all the necessary things we need for testing.

-ifndef(TEST_HRL).
-define(TEST_HRL, 1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_addons/include/eunit_addons.hrl").
-include_lib("mockgyver/include/mockgyver.hrl").

-endif.

-endif.
