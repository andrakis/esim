-module(world).

-export([room/5, map/2, join/3, test/0]).

-define(directions, [north, south, east, west, northeast, southeast, southwest,
	northwest]).
-define(all_directions, ?directions ++ [up, down]).

opposite(north) -> south;
opposite(south) -> north;
opposite(east) -> west;
opposite(west) -> east;
opposite(northeast) -> southwest;
opposite(southeast) -> northwest;
opposite(southwest) -> northeast;
opposite(northwest) -> southeast;
opposite(What) -> error({badarg, What}).

-type pos() :: {integer(), integer()}.

position(Pos, {X, Y}) ->
	case Pos of
		north -> {X, Y - 1};
		south -> {X, Y + 1};
		east  -> {X + 1, Y};
		west  -> {X - 1, Y};
		northeast -> {X + 1, Y - 1};
		southeast -> {X + 1, Y + 1};
		southwest -> {X - 1, Y + 1};
		northwest -> {X - 1, Y - 1};
		_ -> error({badarg, Pos})
	end.

-type direction() :: north | south | east | west | northeast | southeast | southwest | northwest.
-type room_id() :: reference().

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

room(Id, Title, Description, Exits, Tile) ->
	#room{
		id = Id,
		title = Title,
		description = Description,
		exits = Exits,
		tile = Tile
	}.

join(Direction, #room{ id = Id, tile = Tile }, #room{ exits = Exits0 } = Room) ->
	Exits1 = [#exit{
		direction = Direction,
		room_id = Id,
		tile = Tile
	} | lists:keydelete(Direction, #exit.direction, Exits0)],
	Room#room{
		exits = Exits1
	}.

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

vis(Room, Rooms) ->
	#vis{
		rooms = Rooms,
		root = Room
	}.

visualize(#vis{} = Vis0) ->
	Vis1 = map_room(Vis0#vis.root, {0, 0}, Vis0),
	XSeq = lists:seq(Vis1#vis.min_x, Vis1#vis.max_x),
	YSeq = lists:seq(Vis1#vis.min_y, Vis1#vis.max_y),
	[ begin
		[ case lists:keyfind({X, Y}, 1, Vis1#vis.visual) of
			false -> <<".">>;
			{_, Tile} -> Tile
		  end || X <- XSeq ] ++ <<"\n">>
	  end || Y <- YSeq ].

map_room(#room{} = Room, Pos, #vis{} = Vis) ->
	map_room(Room, Pos, Vis, nil).

map_room(#room{} = Room, Pos, #vis{} = Vis, Prev) ->
	IsDone = lists:keyfind(Pos, 1, Vis#vis.done) == {Pos, done},
	maybe_map_room(IsDone, Room, Pos, Vis, Prev).

-define(if_true(Test, New, Old), if Test -> New; true -> Old end).
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
