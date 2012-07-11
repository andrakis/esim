%% @doc Provides some shared definitions.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-ifndef(SHARED_HRL).
-define(SHARED_HRL, 1).

% Since I can't figure out how to set compile options for Syntastic, I'm going
% to go about it here. Note that rebar.config defines no_default_test.
% Actually, whatever. It's not defining it. Let's assume for now that ?TEST does
% not involve altering general execution, it only adds some tests.
-ifndef(no_default_test).
-define(TEST, 1).
-endif.

% rbdict is an opaque type.
-type rbdict() :: empty | term().

-endif.
