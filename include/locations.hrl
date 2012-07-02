%% @doc Provides record structures for locations.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-ifndef(LOCATIONS_HRL).
-define(LOCATIONS_HRL, 1).

-include("include/actors.hrl").

-record(location, {
	% All the locations within this location
	sub_locations = []       :: [pid()],
	% All the actors in this part of the location (not including sub-locations)
	actors = []              :: [pid()],
	% The current iteration reference
	iteration_reference      :: reference(),
	% Iteration callbacks we're still waiting for
	iteration_waitlist = []  :: [pid()],
	% Who asked for this iteration?
	iteration_callee         :: pid(),
	% The module that will handle events
	module = loc             :: atom(),
	% Specific location record goes here
	state                    :: term(),
	% A snapshot of every member in this location
	snapshot = []            :: [term()],
	% The new snapshot as we build it
	snapshot_building = []   :: [term()]
}).
-type location() :: #location{}.

-endif.
