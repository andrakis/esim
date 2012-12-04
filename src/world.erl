-module(world).

-export([room/5, map/2, join/3, opposite/1]).
-export([test/0]).

-define(directions, [north, south, east, west, northeast, southeast, southwest,
	northwest]).
-define(all_directions, ?directions ++ [up, down]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type direction() :: north | south | east | west | northeast | southeast | southwest | northwest.
-type room_id() :: reference().
-type pos() :: {integer(), integer()}.

%% @doc Get the opposite direction to the one given.
-spec opposite(direction()) -> direction().
opposite(north) -> south;
opposite(south) -> north;
opposite(east) -> west;
opposite(west) -> east;
opposite(northeast) -> southwest;
opposite(southeast) -> northwest;
opposite(southwest) -> northeast;
opposite(northwest) -> southeast;
opposite(What) -> error({badarg, What}).

%% @doc Convert the direction in a new position, relative to the one given.
-spec position(direction(), pos()) -> pos().
position(Direction, {X, Y}) ->
	case Direction of
		north -> {X, Y - 1};
		south -> {X, Y + 1};
		east  -> {X + 1, Y};
		west  -> {X - 1, Y};
		northeast -> {X + 1, Y - 1};
		southeast -> {X + 1, Y + 1};
		southwest -> {X - 1, Y + 1};
		northwest -> {X - 1, Y - 1};
		_ -> error({badarg, Direction})
	end.

-record(exit, {
	direction :: direction(),
	room_id :: room_id(),
	tile :: binary()
}).
-type exit() :: #exit{}.

-record(room, {
	id    :: room_id(),
	title :: binary(),
	description = <<"">> :: binary(),
	exits = [] :: [exit()],
	tile = <<" ">> :: binary(),
	art = [] :: [{direction(), binary()}],
	contents = [] :: [term()]
}).
-type room() :: #room{}.

%% @doc Create a new room.
-spec room(Id::term(), Title::binary(), Description::binary(), Exits::[exit()], Tile::binary()) -> room().
room(Id, Title, Description, Exits, Tile) ->
	#room{
		id = Id,
		title = Title,
		description = Description,
		exits = Exits,
		tile = Tile
	}.

%% @doc Create a join from Room2 to Room1 in the given Direction.
%%      This is a one-way join - no join is performed in Room1, unless you manually
%%      perform the reverse call (eg, join(opposite(Direction), Room2, Room1)).
-spec join(Direction::direction(), Room1::room(), Room2::room()) -> room().
join(Direction, #room{ id = Id, tile = Tile }, #room{ exits = Exits0 } = Room) ->
	Exits1 = [#exit{
		direction = Direction,
		room_id = Id,
		tile = Tile
	} | lists:keydelete(Direction, #exit.direction, Exits0)],
	Room#room{
		exits = Exits1
	}.

%% @doc Get the border art in Direction from Room.
%%      If there is no exit in Direction, return tile art in that
%%      direction, or <<".">>.
-spec border(Direction::direction(), Room::room()) -> binary().
border(Direction, #room{ art = Art, exits = [] }) ->
	case lists:keyfind(Direction, 1, Art) of
		false -> <<".">>;
		{_, Tile} -> Tile
	end;
border(Direction, #room{ art = Art, exits = Exits }) ->
	case lists:keyfind(Direction, #exit.direction, Exits) of
		#exit{ tile = Tile } -> Tile;
		false -> border(Direction, #room{ art = Art })
	end.

%% @doc Given starting position Room (0, 0) and a list of Rooms, map
%%      the given data into a list of list of binaries, each list holding
%%      a sub list of binaries, the characters to print.
-spec map(Room::room(), Rooms::[room()]) -> [[binary()]].
map(#room{} = Room, Rooms) ->
	V = vis(Room, Rooms),
	visualize(V).

-record(vis, {
	rooms = [] :: {room_id(), room()},
	root :: room(),
	min_x = -1,
	min_y = -1,
	max_x =  1,
	max_y =  1,
	visual = [] :: [{pos(), binary()}],
	done = [] :: [{pos(), done}]
}).
-type vis() :: #vis{}.

%% @doc Create a new visualization.
-spec vis(Room::room(), Rooms::[room()]) -> vis().
vis(Room, Rooms) ->
	#vis{
		rooms = Rooms,
		root = Room
	}.

%% @doc Visualize and render the given vis.
%% @private
-spec visualize(Vis::vis()) -> [[binary()]].
visualize(#vis{} = Vis0) ->
	Vis1 = map_room(Vis0#vis.root, {0, 0}, Vis0),
	XSeq = lists:seq(Vis1#vis.min_x, Vis1#vis.max_x),
	YSeq = lists:seq(Vis1#vis.min_y, Vis1#vis.max_y),
	[ begin
		[ case lists:keyfind({X, Y}, 1, Vis1#vis.visual) of
			false -> <<".">>;
			{_, Tile} -> Tile
		  end || X <- XSeq ] ++ [<<"\n">>]
	  end || Y <- YSeq ].

%% @see map_room/4
%% @private
-spec map_room(Room::room(), Pos::pos(), Vis::vis()) -> [[binary()]].
map_room(#room{} = Room, Pos, #vis{} = Vis) ->
	map_room(Room, Pos, Vis, nil).

%% @doc Map a given room, and all of its connected rooms.
%% @private
-spec map_room(Room::room(), Pos::pos(), Vis::vis(), Prev::room_id()) -> [[binary()]].
map_room(#room{} = Room, Pos, #vis{} = Vis, Prev) ->
	IsDone = lists:keyfind(Pos, 1, Vis#vis.done) == {Pos, done},
	maybe_map_room(IsDone, Room, Pos, Vis, Prev).

-define(if_true(Test, New, Old), if Test -> New; true -> Old end).

%% @doc Map a room only if it has not been done yet, as specified by IsDone.
%% @private
-spec maybe_map_room(IsDone::boolean(), Room::room(), Pos::pos(), Vis::vis(),
		Prev::room_id()) -> [[binary()]].
maybe_map_room(true,  _,    _,            Vis, _) ->
	Vis;
maybe_map_room(false, Room, {X, Y} = Pos, #vis{ visual = Visual0} = Vis0, Prev0) ->
	io:format("Prev0: ~p~n", [Prev0]),
	Prev1 = if
		Prev0 == nil -> Room#room.id;
		true -> Prev0
	end,
	io:format("Prev: ~p~n", [Prev1]),
	Tile = if
		length(Room#room.contents) > 0 -> list_to_binary(integer_to_list(length(Room#room.contents)));
		true -> Room#room.tile
	end,
	Visual1 = [{Pos, Tile} | Visual0],
	Visual2 = lists:foldl(fun(Direction, VisualAcc) ->
		Position = position(Direction, Pos),
		case lists:keyfind(Position, 1, VisualAcc) of
			false ->
				case lists:keyfind(Direction, #exit.direction, Room#room.exits) of
					false -> VisualAcc;
					Exit ->
						Rm = lists:keyfind(Exit#exit.room_id, #room.id, Vis0#vis.rooms),
						[{Position, border(Direction, Rm)} | VisualAcc]
				end;
			_ ->
				VisualAcc
		end
	end, Visual1, ?directions),
	Vis1 = Vis0#vis{
		done = [{Pos, done} | Vis0#vis.done],
		min_x = ?if_true(X - 1 < Vis0#vis.min_x, X - 1, Vis0#vis.min_x),
		min_y = ?if_true(Y - 1 < Vis0#vis.min_y, Y - 1, Vis0#vis.min_y),
		max_x = ?if_true(X + 1 > Vis0#vis.max_x, X + 1, Vis0#vis.max_x),
		max_y = ?if_true(Y + 1 > Vis0#vis.max_y, Y + 1, Vis0#vis.max_y),
		visual = Visual2
	},
	lists:foldl(fun(Direction, VisAcc) ->
		case lists:keyfind(Direction, #exit.direction, Room#room.exits) of
			#exit{ room_id = Id } when Id /= Prev1 ->
				Rm = lists:keyfind(Id, #room.id, Vis1#vis.rooms),
				Position = position(Direction, Pos),
				map_room(Rm, Position, VisAcc, Prev1);
			_ ->
				VisAcc
		end
	end, Vis1, ?directions).

test() ->
	R1 = room(make_ref(), <<"Start">>, <<>>, [], <<" ">>),
	R2 = room(make_ref(), <<"East">>, <<>>, [], <<" ">>),
	R3 = room(make_ref(), <<"South">>, <<>>, [], <<" ">>),
	R1a = join(east, R2, R1),
	R2a = join(south, R3, R2),
	R2b = join(west, R1, R2a),
	R3a = join(north, R2, R3),
	Rs = [R1a, R2b, R3a],
	io:format("~s~n", [map(R1a, Rs)]).

%%============================================================================
%% Test functions
%%============================================================================
-ifdef(TEST).

join_test() ->
	Ra0 = room(room_a, <<"Test room A">>, <<"Description">>, [], <<" ">>),
	Rb0 = room(room_b, <<"Test room B">>, <<"Description">>, [], <<" ">>),
	Rc0 = room(room_c, <<"Test room C">>, <<"Description">>, [], <<" ">>),

	Ra1 = join(east, Rb0, Ra0),
	?assertMatch(#exit{
		direction = east,
		room_id = room_b
	}, lists:keyfind(east, #exit.direction, Ra1#room.exits)),

	% Joining another room in the same direction overwrites the previous.
	Ra2 = join(east, Rc0, Ra1),
	?assertMatch(#exit{
		direction = east,
		room_id = room_c
	}, lists:keyfind(east, #exit.direction, Ra2#room.exits)),

	ok.

border_test() ->
	Ra0 = room(room_a, <<"Test room A">>, <<>>, [], <<" ">>),
	Ra1 = Ra0#room{
		art = [{east, <<"e">>}, {west, <<"w">>}]
	},

	?assertEqual(<<"e">>, border(east, Ra1)),
	?assertEqual(<<"w">>, border(west, Ra1)),
	?assertEqual(<<".">>, border(north, Ra1)),
	?assertEqual(<<".">>, border(south, Ra1)),

	Ra2 = Ra1#room{
		exits = [#exit{
			direction = south,
			tile = <<"s">>
		}]
	},
	?assertEqual(<<"s">>, border(south, Ra2)),

	ok.

%% TODO: Make this test more low-level.
map_test() ->
	R1 = room(make_ref(), <<"Start">>, <<>>, [], <<"!">>),
	R2 = room(make_ref(), <<"East">>, <<>>, [], <<"2">>),
	R3 = room(make_ref(), <<"South">>, <<>>, [], <<"3">>),
	R1a = join(east, R2, R1),
	R2a = join(south, R3, R2),
	R2b = join(west, R1, R2a),
	R3a = join(north, R2, R3),
	Rs = [R1a, R2b, R3a],

	Map = map(R1a, Rs),
	?assertEqual([
		[<<".">>,<<".">>,<<".">>,<<".">>,<<"\n">>],
		[<<".">>,<<"!">>,<<"2">>,<<".">>,<<"\n">>],
		[<<".">>,<<".">>,<<"3">>,<<".">>,<<"\n">>],
		[<<".">>,<<".">>,<<".">>,<<".">>,<<"\n">>]],
		Map),
	
	ok.

-endif.
