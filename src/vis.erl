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

-include("locations.hrl").

-record(map_entry, {
	pos_2d :: pos_2d(),
	content :: string()
}).
-type map_entry() :: #map_entry{}.

-record(map_done, {
	pos_2d :: pos_2d(),
	location :: location()
}).
-type map_done() :: #map_done{}.

-record(vis, {
	lower_x = 0         :: integer(),
	lower_y = 0         :: integer(),
	upper_x = 0         :: integer(),
	upper_y = 0         :: integer(),
	map = []            :: [map_entry()],
	done = []           :: [map_done()],
	% How far should we travel for mapping? Aborts after this length.
	search_width = all  :: integer() | all,
	search_height = all :: integer() | all,
	module = ?MODULE    :: atom()
}).

-export([handle_render/1, handle_border/2]).


%%============================================================================
%% Reference behaviour implementation
%%============================================================================

%% @doc Given a record of some sort, render it to text.
-spec handle_render(term()) -> string().
handle_render(#location{}) -> " ";
handle_render(#actor{}) -> "@";
handle_render(_) -> "?".

%% @doc Given a direction and location record, render the border.
-spec handle_border(direction_2d(), location()) -> string().
handle_border(_, #location{ neighbours = [] }) ->
	"*";
handle_border(Direction, #location{ neighbours = Neighbours }) ->
	case proplists:get_value(Direction, Neighbours) of
		undefined -> "*";
		#location{} = Loc -> handle_render(Loc)
	end.

%% OTHER
-spec map(InitialLocation::location()) -> #vis{}.
map(InitialLocation) ->
	map(InitialLocation, {0, 0}, #vis{}).

-spec map(Location::location(), pos_2d(), Vis::#vis{}) -> #vis{}.
map(#location{} = Loc, #pos_2d{ x = X, y = Y } = Pos2d, #vis{ map = Map, done = Done } = Vis0) ->
	% Prevent recursion
	case lists:keyfind(Pos2d, #map_done.pos_2d, Done) of
		false ->
			Module = Vis0#vis.module,
			Entry = #map_entry{
				pos_2d = Pos2d,
				content = Module:handle_render(Loc)
			},
			Vis1 = recalculate_boundries(Pos2d, Vis0),
			Vis2 = Vis1#vis{
				map = [Entry | Vis1#vis.map],
				done = [{Pos2d, Loc} | Done]
			},
			% Render the boundries in each direction
			Map0 = Vis2#vis.map,
			Map1 = lists:foldl(fun(Direction, MapAcc) ->
				Pos = world_relative_pos2d(Direction, Pos2d),
				case proplists:get_value(Pos, MapAcc) of
					undefined ->
						[Module:handle_border(Direction, Loc) | MapAcc];
					_ ->
						MapAcc
				end
			end, Map0, ?DIRECTIONS_2D),
			Vis2#vis{
				map = Map1
			};
		_ ->
			Vis0
	end.

recalculate_boundries(#pos_2d{ x = X, y = Y }, #vis{} = Vis0) ->
	Vis0#vis{
		lower_x = min(Vis0#vis.lower_x, X - 1),
		lower_y = min(Vis0#vis.lower_y, Y - 1),
		upper_x = max(Vis0#vis.upper_x, X + 1),
		upper_y = max(Vis0#vis.upper_y, Y + 1)
	}.

-spec world_relative_pos2d(Direction::direction_2d(), pos_2d()) ->
	pos_2d().
world_relative_pos2d(Direction, #pos_2d{ x = X, y = Y } = Pos2d) ->
	case Direction of
		north -> Pos2d#pos_2d{ y = Y - 1 };
		south -> Pos2d#pos_2d{ y = Y + 1 };
		east  -> Pos2d#pos_2d{ x = X + 1 };
		west  -> Pos2d#pos_2d{ x = X - 1 }
	end.

-spec world_relative_pos3d(Direction::direction_3d(), pos_3d()) ->
	pos_3d().
world_relative_pos3d(up, #pos_3d{ z = Z } = Pos3d) ->
	Pos3d#pos_3d{ z = Z + 1 };
world_relative_pos3d(down, #pos_3d{ z = Z } = Pos3d) ->
	Pos3d#pos_3d{ z = Z - 1 };
world_relative_pos3d(Direction, #pos_3d{ xy = XY } = Pos3d) ->
	Pos3d#pos_3d{ xy = world_relative_pos2d(Direction, XY) }.
