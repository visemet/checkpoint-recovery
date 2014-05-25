%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_app).
-behaviour(application).
-compile(no_auto_import).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc TODO document
start(normal, _Args) -> chkrec_sup:start_link().

%% @doc TODO document
stop(_State) -> ok.
