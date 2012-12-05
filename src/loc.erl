%% @doc Implements a generic location.
%%
%% Every conceivable place an actor can be is a location.
%% An empty plot of land is a location, a building is a location.
%% Locations can have many sub-locations - for instance, a building is a
%% location, and each floor is sub-location of that building.
%%
%% Actors have the ability to request entry to another location, and if this
%% is a location that has say, a lock, they are denied access unless they
%% have the key.
%%
%%
%% This is a behaviour module - it expects the a number of callbacks to be
%% implemented by the module provided to new/2.
%% The default behaviour can be relied on or used as a reference implementation.
%%
%% Create
%%   handle_create(InitialState::term()) -> InitialModuleState::term()
%%
%%   Create the initial state for the location.
%%
%% Iterate
%%   handle_iterate(ModuleState::term()) -> NewModuleState::term()
%%
%%   Perform an iteration - do any simulation tasks required. Return the new
%%   module state.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%
-module(loc).
-behavior(gen_server).

-include("gen.hrl").
-include("locations.hrl").

%% Default behaviour callbacks
-export([handle_iterate/1]).
-export([handle_create/1]).
-export([handle_type/1]).
-export([handle_tile/1, handle_border/2]).

%% ESim API
-export([iterate/3]).
-export([behaviour_info/1]).

%% Vis API
-export([position/2, opposite/1]).
-export([join/3]).
-export([tile/1, border/2]).
-export([neighbours/1]).

-export([new/2, stop/1]).

-include("test.hrl").

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Call messages handled
-type call_messages() ::
	% Stop the server
	stop |
	% Create a neighbour join to Location in Direction
	{join, Direction::direction(), Location::pid()} |
	% Get the tile to display for the text representation
	get_tile |
	% Get the border at the given neighbour location
	{get_border, Direction::direction()} |
	% Get the neighbours
	get_neighbours.

%% Call message results
-type call_results() ::
	{reply, call_result_inner(), State::location()} |
	{stop, normal, ok, State::location()}.
-type call_result_inner() ::
	% For a join request
	{join, ok} |             % Success
	{join, error, blocked} | % Neighbour already present
	% A tile request
	{tile, Tile::binary()} |
	% A border tile request
	{border, Border::binary()} |
	% A neighbour request
	{neighbours, [neighbour()]}.

%% Cast messages handled
-type cast_messages() ::
	% Request to iterate
	{iterate, Callee::pid(), Ref::reference()} |
	% The result of an iteraton.
	{iterate_complete, Pid::pid(), Ref::reference(), FinalState::#location{} | #actor{}}.

% The different return values supported for all callbacks.
-type callback_result(State) ::
	% Success. New state is...
	{ok, NewState::State} |
	{sub_location, add | remove, SubLocation::pid(), NewState::State} |
	{actor, add | remove, Actor::pid(), NewState::State}.
-export_type([callback_result/1]).

%%============================================================================
%% Reference behaviour implementation
%%============================================================================

%% @doc Handle the initial creation of the location. The InitialState passed
%%      in from new/2 is passed to this function.
-spec handle_create(InitialState::term()) -> callback_result(term()).
handle_create(InitialState) ->
	% Create your module state based on InitialState
	ModuleState = InitialState,
	{ok, ModuleState}.

%% @doc Handle an iteration.
-spec handle_iterate(State) -> callback_result(State).
handle_iterate(State0) -> {ok, State0}.

%% @doc Handle a request for the type of location this is.
-spec handle_type(State::term()) -> Type::term().
handle_type(_) -> ?loc_generic.

%% @doc Handle a request for the tile of this location.
-spec handle_tile(State::term()) -> binary().
handle_tile(_) -> <<" ">>.

%% @doc Handle a request for the border for neighbour at Direction.
-spec handle_border(Direction::direction(), State::term()) -> binary().
handle_border(_Direction, _State) ->
	% TODO: Get the tile of neighbour at Direction, or return art at Direction.
	<<".">>.

%% Behaviour
-spec behaviour_info(callbacks | term()) -> [{Callback::atom(), Arity::pos_integer()}] | undefined.
behaviour_info(callbacks) ->
	[{handle_create, 0},
	 {handle_iterate, 1},
	 {handle_type, 1}];
behaviour_info(_) ->
	undefined.

%%============================================================================
%% API functions
%%============================================================================

%% @doc Perform an iteration, and instruct all sub locations and actors to
%%      perform their iterations. Calls Callback(Ref) when all such instructed
%%      actors and locations have finished their iterations.
-spec iterate(Pid::pid(), Ref::reference(), Callee::pid()) -> ok.
iterate(Pid, Ref, Callee) ->
	gen_server:cast(Pid, {iterate, Ref, Callee}),
	ok.

%% @doc Complete an iteration.
-spec iterate_complete(Pid::pid(), Ref::reference(), FinalState::term()) -> ok.
iterate_complete(Pid, Ref, FinalState) ->
	gen_server:cast(Pid, {iterate_complete, self(), Ref, FinalState}).

%% @doc Create a new Location. InitialState is passed to
%%      HandlingMoudle:handle_create to receive the initial state.
-spec new(HandlingModule::atom(), InitialState::term()) -> gen_new().
new(HandlingModule, InitialState) ->
	{ok, ModuleState} = HandlingModule:handle_create(InitialState),
	gen_server:start_link(?MODULE, {HandlingModule, ModuleState}, []).

%% @doc Stop the given Location.
-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).

%% @doc Create a join from Location1 to Location2 in the given Direction.
%%      This is a one-way join - no join is performed in Locaiton1, unless you manually
%%      perform the reverse call (eg, join(opposite(Direction), Location2, Location1)).
-spec join(Direction::direction(), Location1::pid(), Location2::pid()) -> ok | {error, blocked}.
join(Direction, Location1, Location2) when is_pid(Location1), is_pid(Location2) ->
	{join, Result} = gen_server:call(Location2, {join, Direction, Location1}),
	Result.

%%============================================================================
%% Relations for visualizations.
%%============================================================================

%% @doc Get the tile for visialization.
-spec tile(pid() | location()) -> binary().
tile(#location{ module = Module, state = State }) ->
	Module:handle_tile(State);
tile(Pid) when is_pid(Pid) ->
	{tile, Tile} = gen_server:call(Pid, get_tile),
	Tile.

%% @doc Get the border tile at the given neighbour.
-spec border(Direction::direction(), pid() | location()) -> binary().
border(Direction, #location{ module = Module, state = State }) ->
	Module:handle_border(Direction, State);
border(Direction, Pid) when is_pid(Pid) ->
	{border, Border} = gen_server:call(Pid, {get_border, Direction}),
	Border.

%% @doc Get the neighbours for the given location pid.
-spec neighbours(pid() | location()) -> [neighbour()].
neighbours(#location{ neighbours = Neighbours }) ->
	Neighbours;
neighbours(Pid) when is_pid(Pid) ->
	{neighbours, Neighbours} = gen_server:call(Pid, get_neighbours),
	Neighbours.

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

%%============================================================================
%% GenServer callbacks
%%============================================================================

%% @doc Initialize the server.
-spec init({HandlingModule::atom(), ModuleState::term()}) -> {ok, #location{}}.
init({HandlingModule, ModuleState}) ->
	process_flag(trap_exit, true),
	Location = #location{
		module = HandlingModule,
		state = ModuleState
	},
	{ok, Location}.

%% @doc Handling call messages
-spec handle_call(call_messages(), From::gen_from(), Location::#location{}) ->
		call_results().
handle_call({join, Direction, Other}, _From, Location0) ->
	#location{ neighbours = Neighbours } = Location0,
	case lists:keyfind(Direction, #neighbour.direction, Neighbours) of
		false ->
			% TODO: Notify handling module
			Location1 = Location0#location{
				neighbours = [#neighbour{
					direction = Direction,
					id = Other
				} | Neighbours]
			},
			{reply, {join, ok}, Location1};
		_ ->
			{reply, {join, {error, blocked}}, Location0}
	end;
handle_call(get_tile, _From, Location) ->
	{reply, {tile, tile(Location)}, Location};
handle_call({get_border, Direction}, _From, Location) ->
	{reply, {border, border(Direction, Location)}, Location};
handle_call(get_neighbours, _From, Location) ->
	{reply, {neighbours, neighbours(Location)}, Location};
handle_call(stop, _From, Location) ->
	{stop, normal, ok, Location}.

-spec handle_cast(cast_messages(), Location0::#location{}) ->
	{noreply, _}.

%% Handle an iteration request. Saves the current snapshot and starts a new iteration.
handle_cast({iterate, Callee, Ref}, #location{ module = Module } = Location0) ->
	Result = Module:handle_iterate(Location0#location.state),
	Location1 = i_handle_callback_result(Result, Location0),
	Location2 = Location1#location{
		iteration_reference = Ref,
		iteration_waitlist = Location1#location.sub_locations ++ Location1#location.actors,
		iteration_callee = Callee,
		snapshot = Location1#location.snapshot_building,
		snapshot_building = []
	},
	[ iterate(Pid, Ref, self()) || Pid <- Location2#location.iteration_waitlist ],
	{noreply, Location2};

%% Handle an iteration complete message
handle_cast({iterate_complete, Pid, Ref, FinalState}, #location{
		iteration_reference = Ref
	} = Location0) ->
	%% TODO: Implement a one-hit-success version?
	Location1 = case lists:partition(fun(APid) -> APid == Pid end, Location0#location.iteration_waitlist) of
		{[], _} ->
			error_logger:error_msg("~p: Received iterate_complete from an unknown member~n", [?MODULE]),
			Location0;
		{[Pid], []} ->
			% Finished!
			SnapshotComplete = [FinalState | Location0#location.snapshot_building],
			iterate_complete(self(), Ref, SnapshotComplete),
			Location0#location{
				iteration_waitlist = [],
				snapshot = SnapshotComplete,
				snapshot_building = []
			};
		{[Pid], Rem} ->
			Location0#location{
				iteration_waitlist = Rem,
				snapshot_building = [FinalState | Location0#location.snapshot_building]
			}
	end,
	{noreply, Location1};
handle_cast({iterate_complete, Pid, _Ref, _FinalState}, Location0) ->
	error_logger:error_msg("~p: Received iterate_complete in wrong reference!~n"
	                       "Offending pid: ~p", [?MODULE, Pid]),
	{noreply, Location0}.

%handle_cast(Request, Location) ->
%	error_logger:error_msg("~p: Unknown cast: ~p~n", [?MODULE, Request]),
%	{noreply, Location}.

%% @doc Handle all the non call/cast messages
-spec handle_info(term(), Location) -> {noreply, Location}.
handle_info(_Msg, Location) ->
	{noreply, Location}.

terminate(Reason, _Location) ->
	error_logger:error_msg("~p: terminate - ~p~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, Location, _Extra) ->
	error_logger:info_msg("~p: Undergoing a code upgrade...", [?MODULE]),
	{ok, Location}.

%%============================================================================
%% Internal functions
%%============================================================================

%% @doc Handle the result of a callback. All callbacks may issue instructions
%%      by returning a type of callback_result/1.
-spec i_handle_callback_result(Result0::callback_result(term()),
	Location0::location()) -> Location1::location().
i_handle_callback_result(Result0, #location{} = Location0) ->
	case Result0 of
		{ok, Result1} ->
			Location0#location{
				state = Result1
			};
		{sub_location, add, Pid, State1} ->
			Location0#location{
				sub_locations = [Pid | Location0#location.sub_locations],
				state = State1
			};
		{sub_location, remove, Pid, State1} ->
			Location0#location{
				sub_locations = Location0#location.sub_locations -- [Pid],
				state = State1
			};
		{actor, add, Pid, State1} ->
			Location0#location{
				actors = [Pid | Location0#location.actors],
				state = State1
			};
		{actor, remove, Pid, State1} ->
			Location0#location{
				actors = Location0#location.actors -- [Pid],
				state = State1
			}
	end.

%%============================================================================
%% Test functions
%%============================================================================
-ifdef(TEST).

-define(CREATE, fun() ->
	{ok, __Instance} = new(?MODULE, []),
	__Instance
end).
-define(DONE, fun(__Instance) -> stop(__Instance) end).

new_test() ->
	?assertMatch({ok, _}, new(?MODULE, undefined)).

tile_test() ->
	L = ?CREATE(),

	?assertEqual(<<" ">>, tile(L)),

	?DONE(L),

	ok.

border_test() ->
	L = ?CREATE(),

	?assertEqual(<<".">>, border(east, L)),

	?DONE(L),

	ok.

join_test() ->
	L = ?CREATE(),
	A = erlang:list_to_pid("<0.123.0>"),
	B = erlang:list_to_pid("<0.123.456>"),

	?assertMatch([], neighbours(L)),

	?assertEqual(ok, join(east, A, L)),
	?assertMatch(#neighbour{
		direction = east,
		id = A
	}, lists:keyfind(east, #neighbour.direction, neighbours(L))),

	% Joining another location in the same direction fails
	?assertEqual({error, blocked}, join(east, B, L)),
	?assertMatch(#neighbour{
		direction = east,
		id = A
	}, lists:keyfind(east, #neighbour.direction, neighbours(L))),

	ok.

neighbours_test() ->
	L = #location{
		neighbours = [a, b, c]
	},

	?assertMatch([a, b, c], neighbours(L)),

	ok.

handle_create_test() ->
	?assertMatch({ok, my_state}, handle_create(my_state)).

handle_iterate_test() ->
	?assertMatch({ok, my_state}, handle_iterate(my_state)).

handle_type_test() ->
	?assertMatch(?loc_generic, handle_type(any_state)).

init_test() ->
	?assertMatch({ok, #location{}}, init({undefined, undefined})).

iterate_test() ->
	Self = self(),
	Location0 = #location{
		sub_locations = [Self]
	},
	Ref = make_ref(),
	?assertMatch({noreply, #location{
		iteration_reference = Ref,
		iteration_waitlist = [Self],
		iteration_callee = Self
	}}, handle_cast({iterate, Self, Ref}, Location0)),
	% Note: normally this would be called from Pid's process. Since we
	% directly called handle_cast, it's actually our pid.
	?assertEqual(ok, receive
		{'$gen_cast', {iterate, Ref, Self}} ->
			ok
		after 1000 ->
			error
	end).

iterate_complete_test() ->
	Self = self(),
	Ref = make_ref(),
	LocationOne = #location{
		iteration_reference = Ref,
		iteration_waitlist = [Self],
		iteration_callee = Self
	},
	LocationMany = LocationOne#location{
		iteration_waitlist = [placeholder | LocationOne#location.iteration_waitlist]
	},
	?assertMatch({noreply, #location{
		iteration_waitlist = [placeholder],
		snapshot_building = [_ | _]
	}}, handle_cast({iterate_complete, Self, Ref, finalstate}, LocationMany)),
	?assertMatch({noreply, #location{
		iteration_waitlist = [],
		snapshot = [finalstate],
		snapshot_building = []
	}}, handle_cast({iterate_complete, Self, Ref, finalstate}, LocationOne)),
	?assertMatch(ok, receive
		{'$gen_cast', {iterate_complete, Self, Ref, [finalstate]}} ->
			ok
		after 1000 ->
			error
	end).

%% @doc Test that the various result to callback_result affect the state as
%%      desired.
callback_result_test() ->
	Self = self(),
	Fun = fun i_handle_callback_result/2,
	State0 = #location{ state = notset },
	?assertMatch(#location{ state = now_set }, Fun({ok, now_set}, State0)),

	% Adding and removing sub locations
	SubLocAdd = Fun({sub_location, add, Self, add_loc}, State0),
	?assertMatch(#location{ state = add_loc, sub_locations = [Self] }, SubLocAdd),
	SubLocRem = Fun({sub_location, remove, Self, rem_loc}, SubLocAdd),
	?assertMatch(#location{ state = rem_loc, sub_locations = [] }, SubLocRem),

	% Adding and removing actor
	ActorAdd = Fun({actor, add, Self, add_act}, State0),
	?assertMatch(#location{ state = add_act, actors = [Self] }, ActorAdd),
	ActorRem = Fun({actor, remove, Self, rem_act}, ActorAdd),
	?assertMatch(#location{ state = rem_act, actors = [] }, ActorRem).
-endif.
