%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_app).
-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc TODO document
start(normal, _Args) -> chkrec_sup:start_link().

%% @doc TODO document
stop(_State) -> ok.
