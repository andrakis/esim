%% @doc Provides record structures for locations.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-ifndef(LOCATIONS_HRL).
-define(LOCATIONS_HRL, 1).

-include("include/actors.hrl").

-record(pos_2d, {
	x :: integer(),
	y :: integer()
}).
-type pos_2d() :: #pos_2d{}.

-record(pos_3d, {
	xy :: pos_2d(),
	z :: integer()
}).
-type pos_3d() :: #pos_3d{}.

-type direction_2d() :: north | south | east | west.
-type direction_3d() :: direction_2d() | up | down.
-define(DIRECTIONS_2D, [north,  south,  east,  west]).
-define(DIRECTIONS_3D, [up,  down | ?DIRECTIONS_2D]).

-record(location, {
	% The name of this location. Should be unique, but no guarantee is given.
	% The name will be registered in the addr_book.
	name                     :: undefined | binary(),
	% Parent location
	parent                   :: undefined | pid(),
	% All the locations within this location
	sub_locations = []       :: [pid()],
	% All the actors in this part of the location (not including sub-locations)
	actors = []              :: [pid()],
	% Neighbouring locations
	neighbours = []          :: [{direction(), #location{}}],
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

% The type of locations available
-define(loc_generic, generic).


-endif.
