%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_keeper_sup).
-behaviour(supervisor).
-compile(no_auto_import).

-export([start_link/0]).

-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc TODO document
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------

%% @doc TODO document
init([]) ->
    ChildSpec = {
        ignored
      , {chkrec_keeper, start_link, []}
      , transient
      , brutal_kill
      , worker
      , [chkrec_keeper]
    }

  , {ok, {{simple_one_for_one, 5, 60}, [ChildSpec]}}
.

%%--------------------------------------------------------------------
