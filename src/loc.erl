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
%% This is a behaviour module - it expects the following callbacks to be
%% implemented by the creating module:
%%
%% Iterate
%%   iterate(ModuleState::term()) -> NewModuleState::term()
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
	% Search for an item
	% XXX(Julian): This was added in the hopes that Dialyzer would complain ...
	%              well, it doesn't. :\
	{search, SearchTerm::term()} |
	% Stop the server
	stop.

%% Cast messages handled
-type cast_messages() ::
	% Request to iterate
	{iterate, Callee::pid(), Ref::reference()} |
	% The result of an iteraton.
	{iterate_complete, Pid::pid(), Ref::reference(), FinalState::#location{} | #actor{}}.

%% Behaviour
-spec behaviour_info(callbacks | term()) -> [{Callback::atom(), Arity::pos_integer()}] | undefined.
behaviour_info(callbacks) ->
	[{iterate, 1}];
behaviour_info(_) ->
	undefined.

%%============================================================================
%% API functions
%%============================================================================

%% @doc Perform an iteration, and instruct all sub locations and actors to
%%      perform their iterations. Calls Callback(Ref) when all such instructed
%%      actors and locations have finished their iterations.
-spec iterate(LocationPid::pid(), Ref::reference(), Callee::pid()) -> ok.
iterate(LocationPid, Ref, Callee) ->
	gen_server:cast(LocationPid, {iterate, Ref, Callee}).

-spec new(HandlingModule::atom(), InitialState::term()) -> gen_new().
new(HandlingModule, InitialState) ->
	gen_server:start_link(?MODULE, {HandlingModule, InitialState}, []).

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).

%%============================================================================
%% GenServer callbacks
%%============================================================================

%% @doc Initialize the server.
-spec init({HandlingModule::atom(), InitialState::term()}) -> {ok, #location{}}.
init({HandlingModule, InitialState}) ->
	process_flag(trap_exit, true),
	Location = #location{
		module = HandlingModule,
		location_state = InitialState
	},
	{ok, Location}.

%% @doc Handling call messages
-spec handle_call(call_messages(), From::gen_from(), Location::#location{}) ->
	{stop, _, _, _}.
handle_call(stop, _From, Location) ->
	{stop, normal, ok, Location}.
%handle_call(Request, _From, Location) ->
%	error_logger:error_msg("~p: Unknown call: ~p~n", [?MODULE, Request]),
%	{reply, error, Location}.

-spec handle_cast(cast_messages(), Location0::#location{}) ->
	{noreply, _}.

%% Handle an iteration request. Saves the current snapshot and starts a new iteration.
handle_cast({iterate, Callee, Ref}, Location0) ->
	Location1 = Location0#location{
		iteration_reference = Ref,
		iteration_waitlist = Location0#location.sub_locations ++ Location0#location.actors,
		iteration_callee = Callee,
		snapshot = Location0#location.snapshot_building,
		snapshot_building = []
	},
	[ iterate(Pid, Ref, self()) || Pid <- Location1#location.iteration_waitlist ],
	{noreply, Location1};

%% Handle an iteration complete message
handle_cast({iterate_complete, Pid, Ref, FinalState}, #location{
		iteration_reference = Ref
	} = Location0) ->
	%% TODO: Implement a one-hit-success version?
	Location1 = case lists:partition(fun(APid) -> APid == Pid end, Location0#location.iteration_waitlist) of
		{[], _} ->
			error_logger:error_msg("~p: Received iterate_complete from an unknown member~n", [?MODULE]),
			Location0;
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

%%============================================================================
%% Test functions
%%============================================================================
-ifdef(TEST).

new_test() ->
	{ok, Pid} = new(?MODULE, undefined),
	stop(Pid).

init_test() ->
	?assertMatch({ok, #location{}}, init({undefined, undefined})).

stop_test() ->
	?assertMatch({stop, normal, ok, #location{}}, handle_call(stop, undefined, #location{})),
	{ok, Pid} = new(?MODULE, undefined),
	stop(Pid),
	?assertEqual(false, is_process_alive(Pid)).

-endif.
