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
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%
-module(loc).
-behavior(gen_server).

-include("include/locations.hrl").

%% The state record this module uses
-define(state, #location).

-export([start_link/0, stop/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Internal functions

%%============================================================================
%% API functions
%%============================================================================

%% @doc Perform an iteration, and instruct all sub locations and actors to
%%      perform their iterations. Calls Callback(Ref) when all such instructed
%%      actors and locations have finished their iterations.
-spec iterate(LocationPid::pid(), Ref::reference(),
      Callback::fun((Ref::reference()) -> _)) -> ok.
iterate(LocationPid, Ref, Callback) when is_reference(Ref), is_function(Callback, 1) ->
	gen_server:cast(LocationPid, {iterate, Ref, Callback}).

-spec start_link() -> ServerPid::pid().
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).

%%============================================================================
%% GenServer callbacks
%%============================================================================

%% @doc Initialize the server.
-spec init([]) -> {ok, ?state{}}.
init([]) ->
	process_flag(trap_exit, true),
	State = ?state{
		snapshot = ?state{}
	},
	{ok, State}.

%% @doc Handling call messages
-spec handle_call(term(), From::{Pid::pid(), Ref::reference()}, State::?state{}) ->
      term().
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(Request, _From, State) ->
	error_logger:error_msg("~p: Unknown call: ~p~n", [?MODULE, Request]),
	{reply, error, State}.

handle_cast(Request, State) ->
	error_logger:error_msg("~p: Unknown cast: ~p~n", [?MODULE, Request]),
	{noreply, State}.

%% @doc Handle all the non call/cast messages
-spec handle_info(term(), State) -> {noreply, State}.
handle_info(_Msg, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	error_logger:error_msg("~p: terminate - ~p~n", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	error_logger:info_msg("~p: Undergoing a code upgrade...", [?MODULE]),
	{ok, State}.

%%============================================================================
%% Internal functions
%%============================================================================


