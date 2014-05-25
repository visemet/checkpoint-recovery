%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_sup).
-behaviour(supervisor).
-compile(no_auto_import).

-define(KEEPER_MON, chkrec_keeper_mon).
-define(KEEPER_SUP, chkrec_keeper_sup).

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
    KeeperMonSpec = {
        ?KEEPER_MON
      , {?KEEPER_MON, start_link, []}
      , transient
      , brutal_kill
      , worker
      , [?KEEPER_MON]
    }

  , KeeperSupSpec = {
        ?KEEPER_SUP
      , {?KEEPER_SUP, start_link, []}
      , transient
      , infinity
      , supervisor
      , [?KEEPER_SUP]
    }

  , ChildSpecs = [KeeperMonSpec, KeeperSupSpec]
  , {ok, {{one_for_all, 1, 60}, ChildSpecs}}
.

%%--------------------------------------------------------------------
