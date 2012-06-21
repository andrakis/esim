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
	sub_lococations = []     :: [#location{}],
	% All the actors in this part of the location (not including sub-locations)
	actors = []              :: [#actor{}],
	% Specific location record goes here
	location_state           :: term(),
	% A snapshot of every member in this location
	snapshot                 :: #location{}
}).

-endif.
