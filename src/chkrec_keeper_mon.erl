%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_keeper_mon).
-behaviour(gen_server).
-compile(no_auto_import).

-define(TIMEOUT, 5000).
-define(KEEPER_SUP, chkrec_keeper_sup).

-export([new/1, update/2, lookup/1, watch/2, start_link/0]).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-export_type([maybe_pid/0]).

-type maybe_pid() :: chkrec_keeper:maybe(pid()) | down.
%% Optionally present pid.

-type registry() :: dict:dict(term(), maybe_pid()).
%% Mapping of identifiers for useful workers to pids of backup keepers.

-type refs() :: dict:dict(reference(), term()).
%% Mapping of references for monitored backup keepers to identifiers for
%% useful workers.

-record(keeper_mon, {
    registry = dict:new() :: registry()
  , refs = dict:new() :: refs()
}).

-type keeper_mon() :: #keeper_mon{
    registry :: registry()
  , refs :: refs()
}.
%% Internal backup keeper monitor state.

%%====================================================================
%% API
%%====================================================================

-spec new(Source :: term()) ->
    {ok, Keeper :: pid()}
  | {error, Reason :: term()}
.

%% @doc TODO document
new(Source) ->
    Result = supervisor:start_child(?KEEPER_SUP, [Source])
  , case Result of
        {ok, Keeper} -> update(Source, Keeper)
      ; _Else -> pass
    end

  , Result
.

-spec update(Source :: term(), Keeper :: pid()) -> ok.

%% @doc TODO document
update(Source, Keeper) when is_pid(Keeper) ->
    gen_server:cast(?MODULE, {update, Source, Keeper})
.

-spec lookup(Source :: term()) ->
    {ok, maybe_pid()}
  | {error, Reason :: term()}
.

%% @doc TODO document
lookup(Source) ->
    gen_server:call(?MODULE, {lookup, Source}, ?TIMEOUT)
.

-spec watch(Source :: term(), Keeper :: pid()) -> ok.

%% @doc TODO document
watch(Source, Keeper) when is_pid(Keeper) ->
    gen_server:cast(?MODULE, {watch, Source, Keeper})
.

%%--------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()}
  | {error, Reason :: term()}
.

%% @doc TODO document
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------

-spec init(Args :: list()) ->
    {ok, State :: keeper_mon()}
  | {stop, Reason :: term()}
.

%% @doc Initializes the internal state of the backup keeper monitor.
init([]) -> {ok, #keeper_mon{}}.

%%--------------------------------------------------------------------

-spec handle_call(
    Request :: term()
  , From :: {pid(), Tag :: term()}
  , State0 :: keeper_mon()
) ->
    {reply, Reply :: term(), State1 :: keeper_mon()}
.

%% @doc TODO document
handle_call({lookup, Source}, _From, State) ->
    Registry = State#keeper_mon.registry
  , MaybeKeeper = case dict:find(Source, Registry) of
        {ok, Option} -> Option
      ; error -> none
    end

  , {reply, {ok, MaybeKeeper}, State}
;

handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

%%--------------------------------------------------------------------

-spec handle_cast(
    Request :: term()
  , State0 :: keeper_mon()
) ->
    {noreply, State1 :: keeper_mon()}
.

%% @doc TODO document
handle_cast({update, Source, Keeper}, State0) ->
    Registry0 = State0#keeper_mon.registry
  , Registry1 = dict:store(Source, Keeper, Registry0)
  , State1 = State0#keeper_mon{registry=Registry1}
  , {noreply, State1}
;

handle_cast({watch, Source, Keeper}, State0) ->
    Refs0 = State0#keeper_mon.refs
  , MonitorRef = erlang:monitor(process, Keeper)
  , Refs1 = dict:store(MonitorRef, Source, Refs0)
  , State1 = State0#keeper_mon{refs=Refs1}
  , {noreply, State1}
;

handle_cast(_Request, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec handle_info(
    Info :: timeout | term()
  , State0 :: keeper_mon()
) ->
    {noreply, State1 :: keeper_mon()}
.

%% @doc TODO document
handle_info(
    {'DOWN', MonitorRef, process, _Keeper, _Reason}
  , State0 = #keeper_mon{registry = Registry0, refs = Refs0}
) ->
    % Assumed that `MonitorRef' is present in the mapping of references.
    Source = dict:fetch(MonitorRef, Refs0)
  , Refs1 = dict:erase(MonitorRef, Refs0)

    % Assumed that `Source' is present in the registry, and is
    % associated with `{value, _Keeper}'.
  , Registry1 = dict:store(Source, down, Registry0)

  , State1 = State0#keeper_mon{registry=Registry1, refs=Refs1}
  , {noreply, State1}
;

handle_info(_Info, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec terminate(Reason :: term(), State :: keeper_mon()) -> ok.

%% @doc Called by a `gen_server' when it is about to terminate.
%%      Nothing to clean up though.
terminate(_Reason, _State) -> ok.

-spec code_change(
    OldVsn :: term()
  , State0 :: keeper_mon()
  , Extra :: term()
) ->
    {ok, State1 :: keeper_mon()}
.

%% @doc Called by a `gen_server' when it should update its internal
%%      state during a release upgrade or downgrade.
%%      Nothing to change though.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
