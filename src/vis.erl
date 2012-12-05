%% @doc Simulator visualizer.
%%
%% This module provides the base for a visualizer to render the world.
%% It requires an implementation to work with.
%%
%% Current ideas for visualizers: http interface, console output
%%
%% This module, the base implementation, implements a text mapping that may be
%% printed to console.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-module(vis).

-export([new/2, visualize/1]).

-include("locations.hrl").

-include("test.hrl").

-type loc() :: location_id() | location().
-export_type([loc/0]).

-type visual_map() :: [{pos(), binary()}].
-type map_location_result() :: {Location::loc(), Pos::pos(), Prev::location_id()}.

-record(vis, {
	locations = [] :: {location_id(), loc()},
	root           :: loc(),
	min_x = -1     :: neg_integer(),
	min_y = -1     :: neg_integer(),
	max_x =  1     :: pos_integer(),
	max_y =  1     :: pos_integer(),
	visual = []    :: visual_map(),
	done = []      :: [{pos(), done}]
}).
-type vis() :: #vis{}.

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new visualization.
-spec new(Location::loc(), Locations::[loc()]) -> vis().
new(Location, Locations) ->
	#vis{
		locations = Locations,
		root = Location
	}.

%% @doc Visualize and render the given vis.
%% @private
-spec visualize(Vis::vis()) -> [[binary()]].
visualize(#vis{} = Vis0) ->
	{Vis1, Actions} = map_location(Vis0#vis.root, {0, 0}, Vis0),
	Vis2 = map_loop(Vis1, Actions),
	XSeq = lists:seq(Vis2#vis.min_x, Vis2#vis.max_x),
	YSeq = lists:seq(Vis2#vis.min_y, Vis2#vis.max_y),
	[ begin
		[ case lists:keyfind({X, Y}, 1, Vis2#vis.visual) of
			false -> <<".">>;
			{_, Tile} -> Tile
		  end || X <- XSeq ] ++ [<<"\n">>]
	  end || Y <- YSeq ].

%%============================================================================
%% Reference behaviour implementation
%%============================================================================

%%============================================================================
%% Internal functions
%%============================================================================

%% @doc Recusrively perform the map_loop calls required.
-spec map_loop(Vis::vis(), Actions::[{Location::loc(), Position::pos(), Prev::location_id()}]) -> [[binary()]].
map_loop(Vis0, [{Location, Position, Prev} | Tail]) ->
	{Vis1, Actions} = map_location(Location, Position, Vis0, Prev),
	map_loop(Vis1, Actions ++ Tail);
map_loop(Vis0, _) ->
	Vis0.

%% @see map_location/4
%% @private
-spec map_location(Location::loc(), Pos::pos(), Vis::vis()) -> map_location_result().
map_location(Location, Pos, #vis{} = Vis) ->
	map_location(Location, Pos, Vis, nil).

%% @doc Map a given location, and all of its connected locations.
%% @private
-spec map_location(Location::loc(), Pos::pos(), Vis::vis(), Prev::location_id()) ->
		map_location_result().
map_location(Location, Pos, #vis{} = Vis, Prev) ->
	IsDone = lists:keyfind(Pos, 1, Vis#vis.done) == {Pos, done},
	maybe_map_location(IsDone, Location, Pos, Vis, Prev).

-define(if_true(Test, New, Old), if Test -> New; true -> Old end).

%% @doc Map a location only if it has not been done yet, as specified by IsDone.
%% @private
-spec maybe_map_location(IsDone::boolean(), Location::loc(), Pos::pos(), Vis::vis(),
		Prev::location_id()) -> map_location_result().
maybe_map_location(true,  _,    _,            Vis, _) ->
	{Vis, []};
maybe_map_location(false, Location, {X, Y} = Pos, #vis{ visual = Visual0 } = Vis0, Prev0) ->
	Prev1 = if
		Prev0 == nil -> ?location_id(Location);
		true -> Prev0
	end,
	Tile = loc:tile(Location),
	Visual1 = [{Pos, Tile} | Visual0],
	Visual2 = maybe_map_borders(?directions, Location, Pos, Vis0, Visual1),
	Vis1 = Vis0#vis{
		done = [{Pos, done} | Vis0#vis.done],
		min_x = ?if_true(X - 1 < Vis0#vis.min_x, X - 1, Vis0#vis.min_x),
		min_y = ?if_true(Y - 1 < Vis0#vis.min_y, Y - 1, Vis0#vis.min_y),
		max_x = ?if_true(X + 1 > Vis0#vis.max_x, X + 1, Vis0#vis.max_x),
		max_y = ?if_true(Y + 1 > Vis0#vis.max_y, Y + 1, Vis0#vis.max_y),
		visual = Visual2
	},
	Actions = lists:foldl(fun(Direction, ActionsAcc) ->
		case loc:neighbour(Direction, Location) of
			#neighbour{ id = Id } when Id /= Prev1 ->
				{_, Neighbour} = lists:keyfind(Id, 1, Vis1#vis.locations),
				Position = loc:position(Direction, Pos),
				[{Neighbour, Position, Prev1} | ActionsAcc];
			_ ->
				ActionsAcc
		end
	end, [], ?directions),
	{Vis1, Actions}.

%% @doc Map the borders around Location, unless a location is defined at Pos.
%% @private
-spec maybe_map_borders([direction()], Location::loc(), Pos::pos(), Vis::vis(),
		Visual::visual_map()) -> visual_map().
maybe_map_borders([Direction | Tail], Location, Pos, Vis, Visual0) ->
	Position = loc:position(Direction, Pos),
	Visual1 = case lists:keyfind(Position, 1, Visual0) of
		false ->
			case loc:neighbour(Direction, Location) of
				#neighbour{ id = Id } ->
					Neighbour = lists:keyfind(Id, 1, Vis#vis.locations),
					[{Position, loc:border(Direction, Neighbour)} | Visual0];
				_ ->
					Visual0
			end;
		_ ->
			Visual0
	end,
	maybe_map_borders(Tail, Location, Pos, Vis, Visual1);
maybe_map_borders([], _Location, _Pos, _Vis, Visual) ->
	Visual.

%%============================================================================
%% Test functions
%%============================================================================

-ifdef(TEST).

% Required to use Mockgyver.
vis_test_() ->
	?WITH_MOCKED_SETUP(fun setup/0, fun cleanup/1).

setup() ->
	ok.

cleanup(_) ->
	ok.

%% TODO: maybe_map_borders_test.

%% TODO: Make this test more low-level.
visualize_test(_) ->

	% Simulate locations as best we can
	L1 = list_to_pid("<0.1.0>"),
	L2 = list_to_pid("<0.2.0>"),
	L3 = list_to_pid("<0.3.0>"),
	Ls = [ {L, L} || L <- [L1, L2, L3] ],

	?WHEN(loc:neighbour(Dir, L) -> case Dir of
		% East points L1 to L2
		east when L == L1 -> #neighbour{ id = L2 };
		% West points L2 to L1
		west when L == L2 -> #neighbour{ id = L1 };
		% South points L2 to L3
		south when L == L2 -> #neighbour{ id = L3 };
		% North points L3 to L2
		north when L == L3 -> #neighbour{ id = L2 };
		_ -> false
	end),
	?WHEN(loc:tile(L) -> case L of
		L1 -> <<"!">>;
		L2 -> <<"2">>;
		L3 -> <<"3">>;
		_ -> <<" ">>
	end),
	?WHEN(loc:border(_, _) -> <<".">>),

	Vis = new(L1, Ls),
	Map = visualize(Vis),
	?assertEqual([
		[<<".">>,<<".">>,<<".">>,<<".">>,<<"\n">>],
		[<<".">>,<<"!">>,<<"2">>,<<".">>,<<"\n">>],
		[<<".">>,<<".">>,<<"3">>,<<".">>,<<"\n">>],
		[<<".">>,<<".">>,<<".">>,<<".">>,<<"\n">>]],
		Map),
	
	?debugFmt("Pretty: ~s~n", [Map]),
	
	ok.

-endif.
