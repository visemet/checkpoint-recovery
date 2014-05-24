%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_keeper_mon).
-behaviour(gen_server).

-define(TIMEOUT, 5000).
-define(KEEPER_SUP, chkrec_keeper_sup).

-export([new/1, update/2, lookup/1, start_link/0]).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-export_type([maybe_pid/0]).

-type maybe_pid() :: chkrec_keeper:maybe(pid()).

-type registry() :: dict:dict(term(), maybe_pid()).
%% Mapping of identifiers for useful workers to pids of backup keepers.

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

%%--------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()}
  | {error, Reason :: term()}
.

%% @doc TODO document
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------

-spec init(Args :: list()) ->
    {ok, State :: registry()}
  | {stop, Reason :: term()}
.

%% @doc Initializes the internal state of the backup keeper monitor.
init([]) -> {ok, dict:new()}.

%%--------------------------------------------------------------------

-spec handle_call(
    Request :: term()
  , From :: {pid(), Tag :: term()}
  , State0 :: registry()
) ->
    {reply, Reply :: term(), State1 :: registry()}
.

%% @doc TODO document
handle_call({lookup, Source}, _From, State) ->
    MaybeKeeper = case dict:find(Source, State) of
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
  , State0 :: registry()
) ->
    {noreply, State1 :: registry()}
.

%% @doc TODO document
handle_cast({update, Source, Keeper}, State0) ->
    State1 = dict:store(Source, Keeper, State0)
  , {noreply, State1}
;

handle_cast(_Request, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec handle_info(
    Info :: timeout | term()
  , State0 :: registry()
) ->
    {noreply, State1 :: registry()}
.

%% @doc TODO document
handle_info(_Info, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec terminate(Reason :: term(), State :: registry()) -> ok.

%% @doc Called by a `gen_server' when it is about to terminate.
%%      Nothing to clean up though.
terminate(_Reason, _State) -> ok.

-spec code_change(
    OldVsn :: term()
  , State0 :: registry()
  , Extra :: term()
) ->
    {ok, State1 :: registry()}
.

%% @doc Called by a `gen_server' when it should update its internal
%%      state during a release upgrade or downgrade.
%%      Nothing to change though.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
