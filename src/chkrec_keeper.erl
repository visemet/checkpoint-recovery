%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO document
-module(chkrec_keeper).
-behaviour(gen_server).
-compile(no_auto_import).

-define(TIMEOUT, 5000).

-export([get/1, put/2, start_link/1]).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-export_type([maybe/1]).

-type maybe(Value) :: {value, Value} | none.
%% Optionally present value.

-record(keeper, {
    source :: term()
  , contents = none :: maybe(term())
}).

-type keeper() :: #keeper{
    source :: term()
  , contents :: maybe(term())
}.
%% Internal backup keeper state.

%%====================================================================
%% API
%%====================================================================

-spec get(Keeper :: pid()) ->
    {ok, maybe(term())}
  | {error, Reason :: term()}
.

%% @doc TODO document
get(Keeper) when is_pid(Keeper) ->
    try
        gen_server:call(Keeper, {get}, ?TIMEOUT)
    catch
        exit:{timeout, _} -> {error, timeout}
    end
.

-spec put(Keeper :: pid(), Value :: term()) ->
    ok
  | {error, Reason :: term()}
.

%% @doc TODO document
put(Keeper, Value) when is_pid(Keeper) ->
    try
        gen_server:call(Keeper, {put, Value}, ?TIMEOUT)
    catch
        exit:{timeout, _} -> {error, timeout}
    end
.

%%--------------------------------------------------------------------

-spec start_link(Source :: term()) ->
    {ok, Pid :: pid()}
  | {error, Reason :: term()}
.

%% @doc TODO document
start_link(Source) -> gen_server:start_link(?MODULE, [Source], []).

%%--------------------------------------------------------------------

-spec init(Args :: list()) ->
    {ok, State :: keeper()}
  | {stop, Reason :: term()}
.

%% @doc Initializes the internal state of the backup keeper.
init([Source]) ->
    chkrec_keeper_mon:update(Source, erlang:self())
  , {ok, #keeper{source=Source}}
.

%%--------------------------------------------------------------------

-spec handle_call(
    Request :: term()
  , From :: {pid(), Tag :: term()}
  , State0 :: keeper()
) ->
    {reply, Reply :: term(), State1 :: keeper()}
.

%% @doc TODO document
handle_call({put, Value}, _From, State0) ->
    State1 = State0#keeper{contents={value, Value}}
  , {reply, ok, State1}
;

handle_call({get}, _From, State) ->
    {reply, {ok, State#keeper.contents}, State}
;

handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

%%--------------------------------------------------------------------

-spec handle_cast(
    Request :: term()
  , State0 :: keeper()
) ->
    {noreply, State1 :: keeper()}
.

%% @doc TODO document
handle_cast(_Request, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec handle_info(
    Info :: timeout | term()
  , State0 :: keeper()
) ->
    {noreply, State1 :: keeper()}
.

%% @doc TODO document
handle_info(_Info, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec terminate(Reason :: term(), State :: keeper()) -> ok.

%% @doc Called by a `gen_server' when it is about to terminate.
%%      Nothing to clean up though.
terminate(_Reason, _State) -> ok.

-spec code_change(
    OldVsn :: term()
  , State0 :: keeper()
  , Extra :: term()
) ->
    {ok, State1 :: keeper()}
.

%% @doc Called by a `gen_server' when it should update its internal
%%      state during a release upgrade or downgrade.
%%      Nothing to change though.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
