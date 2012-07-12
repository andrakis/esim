%% @doc Provides a generic Address Book, similar to the registered process
%       functionality built into Erlang.
%%
%% The idea with this module is that a globally accessible address book is
%% available, but it has a very broad scope and may or may not contain
%% every state available.
%%
%% Searching is perform by way of a path. For example, an actor wishing to
%% find location townhall cellar, could search for: <<"townhall/cellar">>.
%% This could result in a partial match on <<"townhall">>, which would
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
%% Further TODO: It occurs to me that Erlang dictionaries probably don't branch
%% out the way a binary tree would - that is, searching for <<"foo">> and
%% <<"foa">> are probably both somewhat slow, instead of there simply being two
%% leaves with <<"fo">> as the parent and <<"o">> and <<"a">> as children.
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

%% @doc Test that we can add a single item and successfully get it back via
%%      direct lookup.
add_test() ->
	{ok, Book} = start_local(),
	MyPid = self(),
	ok = insert(<<"test">>, MyPid, Book),
	% insert is a cast, wait until the server has finished
	sync(Book),
	% We should expect to get a list of results, in all cases.
	?assertEqual([{found, MyPid}], lookup(<<"test">>, Book)),
	stop(Book).

%% @doc Test that adding multiple items for the same name will give us multiple
%%      results.
%%      This is important, since we may have multiple actors with the same name,
%%      as you would with any phone book. Further information is only gleaned
%%      from the actor/location in question.
add_multi_test() ->
	{ok, Book} = start_local(),
	MyPid = self(),
	AnotherPid = test_other_pid(),
	ok = insert(<<"test">>, MyPid, Book),
	ok = insert(<<"test">>, AnotherPid, Book),
	% insert is a cast, wait until the server has finished
	sync(Book),
	% This time, we should get both results. No guarantee is given on the order
	% results, however.
	Result = lookup(<<"test">>, Book),
	?assertEqual(2, length(Result)),
	% Search for the results via lists:keyfind using the expected result, where
	% each result is {found, Pid::pid()}.
	?assertEqual({value, {found, MyPid}}, lists:keyfind(MyPid, 2, Result)),
	?assertEqual({value, {found, AnotherPid}}, lists:keyfind(AnotherPid, 2, Result)),
	AnotherPid ! stop,
	stop(Book).

%% @doc Test that searching for a partial match works. A partial match is where
%%      you search for <<"test/floor 1">>, and only <<"test">> exists in the
%%      address book. You would then be directed to <<"test">>'s Pid to further
%%      query them for <<"test/floor 1">> which they have a record of.
lookup_partial_test() ->
	% The partial match address book
	{ok, Book} = start_local(),
	MyPid = self(),
	ok = insert(<<"test">>, MyPid, Book),
	% insert is a cast, wait until the server has finished
	sync(Book),

	% Requesters for <<"test*">> will be redirected here, create our own book
	% further lookups
	{ok, MyBook} = start_local(),
	AnotherPid = test_other_pid(),
	{ok, TestBook} = start_local(),
	ok = insert(<<"test/a">>, MyPid, MyBook),
	ok = insert(<<"test/b">>, AnotherPid, MyBook),
	% insert is a cast, wait until the server has finished
	sync(MyBook),

	% We should be redirected to (possibly) multiple other processes
	Result = lookup(<<"test/a">>, Book),
	?assertEqual([{see, MyBook}], lookup(<<"test/a">>, Book)),
	?assertEqual([{see, MyBook}], lookup(<<"test/b">>, Book)),

	% Now that we've been redirected, we should be able to find what we're
	% seeking
	ResultA = lookup(<<"test/a">>, MyBook),
	ResultB = lookup(<<"test/b">>, MyBook),
	?assertEqual({value, {found, MyPid}}, lists:keyfind(MyPid, 2, ResultA)),
	?assertEqual({value, {found, AnotherPid}}, lists:keyfind(AnotherPid, 2, ResultB)).


%% @doc Helper function to create another process that simply waits for 'stop'.
-spec test_other_pid() -> pid().
test_other_pid() ->
	spawn_link(fun() ->
		receive
			stop -> ok
		end
	end).

-endif.
