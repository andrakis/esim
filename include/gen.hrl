%% @doc Provides types for the various standard gen modules (server, fsm, etc)
%%
%% These are provided so that we may properly type everything.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%
%% TODO: Dialyzer doesn't like using these if you don't return them all.

-ifndef(GEN_HRL).
-define(GEN_HRL, 1).

-type gen_timeout() :: pos_integer() | infinity.
-type gen_from()    :: {From::pid(), Ref::term()}.
-type gen_new()     ::
	{ok, Pid::pid()} |
	ignore |
	{error, {already_started, ExistingPid::pid()} | term()}.

%% Gen Server types
-type gen_server_init(State) ::
	{ok, State} | {ok, State, Timeout::integer()} |
	{ok, State, hibernate} |
	{stop, Reason::term()}.
-type gen_server_call(State) ::
	{reply, Reply::term(), NewState::State} |
	{reply, Reply::term(), NewState::State, gen_timeout() | hibernate} |
	{noreply, NewState::State} |
	{noreply, NewState::State, gen_timeout() | hibernate} |
	{stop, Reason::term(), Reply::term(), NewState::State} |
	{stop, Reason::term(), NewState::State}.
-type gen_server_cast(State) ::
	{noreply, NewState::State} |
	{noreply, NewState::State, gen_timeout() | hibernate} |
	{stop, Reason::term(), Reply::term(), NewState::State} |
	{stop, Reason::term(), NewState::State}.

-endif.
