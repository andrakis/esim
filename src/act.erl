%% @doc Implements a basic actor.
%%
%% An actor is any entity that can be in a location, such as people, doors,
%% etc - anything that can be interacted with, or interacts with other actors.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-module(act).
-behavior(gen_server).

-include("gen.hrl").
-include("actors.hrl").

-export([new/2, stop/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Internal functions

%%============================================================================
%% API functions
%%============================================================================

%% @doc Create a new Actor. InitialState is passed to
%%      HandlingMoudle:handle_create to receive the initial state.
-spec new(HandlingModule::atom(), InitialState::term()) -> gen_new().
new(HandlingModule, InitialState) ->
	{ok, ModuleState} = HandlingModule:handle_create(InitialState),
	gen_server:start_link(?MODULE, {HandlingModule, ModuleState}, []).

%% @doc Stop the given Actor.
-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).

%%============================================================================
%% GenServer callbacks
%%============================================================================

%% @doc Initialize the server.
-spec init([]) -> {ok, actor()}.
init([]) ->
	process_flag(trap_exit, true),
	State = #actor{},
	{ok, State}.

%% @doc Handling call messages
-spec handle_call(term(), From::{Pid::pid(), Ref::reference()}, State::actor()) ->
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


%%============================================================================
%% Test functions
%%============================================================================
