%% @doc ESim application.
%%
%% @author Julian "Andrakis" Thatcher <julian@noblesamurai.com>
%% @version 0.0.1
%%

-module(esim).
-export([start/0, stop/0, get_env/1, get_env/2]).

%% @doc Start the simulator.
-spec start() -> ok.
start() ->
	application:start(esim).

%% @doc Stop the simulator.
-spec stop() -> ok.
stop() ->
	application:stop(esim).

%% @doc Get an application environment settings. Returns undefined if not set.
-spec get_env(Setting::atom()) -> term() | undefined.
get_env(Setting) ->
	get_env(Setting, undefined).

%% @doc Get an application environment setting. Return Default if not set.
-spec get_env(Setting::atom(), Default) -> Value::term() | Default.
get_env(Setting, Default) ->
	case application:get_env(esim, Setting) of
		{ok, Value} -> Value;
		_ -> Default
	end.
