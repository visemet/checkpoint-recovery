-module(chkrec_app).
-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, _Args) ->
    % TODO implement
    {error, not_supported_yet}
.

stop(_State) -> ok.
