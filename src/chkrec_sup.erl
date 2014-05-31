%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc The checkpoint-recovery top-level supervisor module.
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

-spec start_link() ->
    {ok, Pid :: pid()}
  | {error, Reason :: term()}
.

%% @doc Convenience function to create the top-level supervisor of the
%%      application, registered locally as `chkrec_sup'.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------

-spec init(Args :: []) ->
    {ok, {{RestartStrategy, MaxR, MaxT}, [ChildSpec]}}
  when
    RestartStrategy :: supervisor:strategy()
  , MaxR :: non_neg_integer()
  , MaxT :: pos_integer()
  , ChildSpec :: supervisor:child_spec()
.

%% @doc Defines the restart strategy, maximum restart frequency and
%%      child specifications of the supervisor.
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
