%% @doc Provides record structures for actors.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-ifndef(ACTORS_HRL).
-define(ACTORS_HRL, 1).

-record(actor, {
	% For quick reference, link to the location the actor is within
	location        :: pid(),
	% Specific actor record goes here
	actor_state     :: term()
}).

-endif.

