%% @doc Provides record structures for locations.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-ifndef(LOCATIONS_HRL).
-define(LOCATIONS_HRL, 1).

-include("include/actors.hrl").

-type direction() :: north | south | east | west | northeast | southeast | southwest | northwest.
-type pos() :: {X::integer(), Y::integer()}.
-type pos3d() :: {XY::pos(), Z::integer()}.

-define(directions, [north, south, east, west, northeast, southeast, southwest, northwest]).
-define(all_directions, ?directions ++ [up, down]).

-record(neighbour, {
	direction :: direction(),
	id :: location_id()
}).
-type neighbour() :: #neighbour{}.

-type location_id() :: pid().

-record(location, {
	% The unique id of this location.
	id                       :: location_id(),
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
	neighbours = []          :: [neighbour()],
	% The current iteration reference
	iteration_reference      :: reference(),
	% Iteration callbacks we're still waiting for
	iteration_waitlist = []  :: [pid()],
	% Who asked for this iteration?
	iteration_callee         :: pid(),
	% Specific location record goes here
	state                    :: term(),
	% A snapshot of every member in this location
	snapshot = []            :: [term()],
	% The new snapshot as we build it
	snapshot_building = []   :: [term()],
	% The function lookup table, opaque type loc:#lookup{}
	lookup
}).
-type location() :: #location{}.

% The type of locations available
-define(loc_generic, generic).

% Get the location id when passed a location id or a location.
-define(location_id(__Loc), (case __Loc of
	__Pid when is_pid(__Pid) -> __Pid;
	#location{ id = __Pid } -> __Pid
end)).

-endif.
