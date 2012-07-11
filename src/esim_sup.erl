
-module(esim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Services that must be started
-define(SERVICES, [{worker, addr_book, start_local, []}]).

%% Service start options
-define(RESTART, permanent).
-define(SHUTDOWN, 2000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Services = [
		{Svc, {Svc, Init, Params},
			?RESTART, ?SHUTDOWN, Type, [Svc]}
	|| {Type, Svc, Init, Params} <- ?SERVICES ],
	{ok, {{one_for_all, 10, 5}, Services}}.

