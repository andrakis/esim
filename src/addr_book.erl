%% @doc Provides a generic Address Book, similar to the registered process
%       functionality built into Erlang.
%%
%% The idea with this module is that a globally accessible address book is
%% available, but it has a very broad scope and may or may not contain
%% every state available.
%%
%% Searching is perform by way of a path. For example, an actor wishing to
%% find location townhall cellar, could search with path: [<<"townhall">>,
%% <<"cellar">>]. The search for the top level would succeed, and the response
%% would direct the searchee to <<"townhall">>'s Pid.
%% The actor may then enter <<"townhall">> and consult the address book there.
%%
%%
%% Further, an actor may have their own address book to store the results
%% of their lookups and for more generically named terms such as home.
%%
%%
%% TODO: This registers globally, what is the impact of that when another node
%%       starts up?
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-module(addr_book).
-behavior(gen_server).

-include("shared.hrl").
-include("gen.hrl").

-export([start_global/0, start_local/0, stop_global/0, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	is_global       :: boolean(),
	dict            :: rbdict() | dict(),
	module = rbdict :: rbdict | dict
}).
-type state()       :: #state{}.

% Call messages handled
-type call_messages() ::
	% Register a name. Duplicates are allowed (multiple results returned)
	{register, Name::binary(), Pid::pid(), Parent::pid() | undefined} |
	% Find all instances of a name.
	{find, [SearchPath::binary()]} |
	test.

-type register_result() :: ok | error.
-type find_result() :: {found, Pid::pid()} | {see, Pid::pid(), RemainingQuery::[binary()]} | error.
-export_type([register_result/0, find_result/0]).

% Cast messages handled
-type cast_messages() ::
	none.

%%============================================================================
%% API functions
%%============================================================================

-spec start_global() -> {ok, Pid::pid()}.
start_global() ->
	State = #state{
		is_global = true
	},
	gen_server:start_link({global, ?SERVER}, ?MODULE, State, []).

-spec start_local() -> {ok, Pid::pid()}.
start_local() ->
	State = #state{
		is_global = false
	},
	gen_server:start_link(?MODULE, State, []).

-spec stop_global() -> _.
stop_global() ->
	gen_server:cast({global, ?SERVER}, stop).

-spec stop(Pid::pid()) -> _.
stop(Pid) ->
	gen_server:cast(Pid, stop).

%%============================================================================
%% GenServer callbacks
%%============================================================================

%% @doc Initialize the server.
-spec init(State::state()) -> {ok, State::state()}.
init(#state{} = State) ->
	process_flag(trap_exit, true),
	{ok, State}.

%% @doc Handling call messages
-spec handle_call(call_messages(), From::gen_from(), State::#state{}) ->
	{stop, _, _, _}.

handle_call(test, _From, State) ->
	{reply, test, State};

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

-spec handle_cast(cast_messages(), State0::#state{}) ->
	{noreply, _}.

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
-ifdef(TEST).

global_test() ->
	start_global(),
	?assertEqual(test, gen_server:call({global, ?SERVER}, test)),
	stop_global().

local_test() ->
	{ok, Pid} = start_local(),
	?assertMatch(test, gen_server:call(Pid, test)),
	stop(Pid).

-endif.
