%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc A module that supervises the `chkrec_keeper' processes.
-module(chkrec_keeper_sup).
-behaviour(supervisor).
-compile(no_auto_import).

-export([start_link/0]).

-export([init/1]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() ->
    {ok, Pid :: pid()}
  | {error, Reason :: term()}
.

%% @doc Convenience function to create a backup keeper supervisor as
%%      part of a supervisor hierarchy, registered locally as
%%      `chkrec_keeper_sup'.
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
