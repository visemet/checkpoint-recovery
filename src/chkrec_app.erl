%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc The checkpoint-recovery application module.
-module(chkrec_app).
-behaviour(application).
-compile(no_auto_import).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(StartType :: normal, StartArgs :: term()) ->
    {ok, Pid :: pid()}
  | {ok, Pid :: pid(), State :: term()}
  | {error, Reason :: term()}
.

%% @doc Starts the top-level supervisor of the application.
start(normal, _Args) -> chkrec_sup:start_link().

-spec stop(State :: term()) -> no_return().

%% @doc Called whenever the application has stopped.
%%      Nothing to clean up though.
stop(_State) -> ok.
