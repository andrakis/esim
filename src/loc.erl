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
%%   handle_create() -> InitialModuleState::term()
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

-include("include/gen.hrl").
-include("include/locations.hrl").

%% Default behaviour callbacks
-export([handle_iterate/1]).
-export([handle_create/1]).

%% ESim API
-export([iterate/3]).
-export([behaviour_info/1]).

-export([new/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Call messages handled
-type call_messages() ::
	% Stop the server
	stop.

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
	{stop, Error::term(), FinalState::State} |
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

%% Behaviour
-spec behaviour_info(callbacks | term()) -> [{Callback::atom(), Arity::pos_integer()}] | undefined.
behaviour_info(callbacks) ->
	[{handle_create, 0},
	 {handle_iterate, 1}];
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

-spec new(HandlingModule::atom(), InitialState::term()) -> gen_new().
new(HandlingModule, InitialState) ->
	{ok, ModuleState} = HandlingModule:handle_create(InitialState),
	gen_server:start_link(?MODULE, {HandlingModule, ModuleState}, []).

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).

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
	{stop, _, _, _}.
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
			LocationTmp = Location0#location{
				iteration_waitlist = [],
				snapshot = [FinalState | Location0#location.snapshot_building],
				snapshot_building = []
			},
			iterate_complete(self(), Ref, LocationTmp#location.snapshot),
			LocationTmp;
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
			}
	end.

%%============================================================================
%% Test functions
%%============================================================================
-ifdef(TEST).

new_test() ->
	?assertMatch({ok, _}, new(?MODULE, undefined)).

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
	Fun = fun i_handle_callback_result/2,
	State0 = #location{ state = notset },
	?assertMatch(#location{ state = now_set }, Fun({ok, now_set}, State0)).

-endif.
