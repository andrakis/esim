%% @doc
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-module(location).
-behavior(gen_server).

-export([start_link/0, stop/0]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
}).
-type state() :: #state{}.

%% Internal functions

%%============================================================================
%% API functions
%%============================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, stop).

%%============================================================================
%% GenServer callbacks
%%============================================================================

%% @doc Initialize the server.
-spec init([]) -> {ok, state()}.
init([]) ->
	process_flag(trap_exit, true),
	State = #state{},
	{ok, State}.

%% @doc Handling call messages
-spec handle_call(term(), From::{Pid::pid(), Ref::reference()}, State::state()) ->
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


