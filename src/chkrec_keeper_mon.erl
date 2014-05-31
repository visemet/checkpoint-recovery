%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc A module that monitors the status of `chkrec_keeper' processes.
-module(chkrec_keeper_mon).
-behaviour(gen_server).
-compile(no_auto_import).

-define(TIMEOUT, 5000).
-define(KEEPER_SUP, chkrec_keeper_sup).

-export([new/1, update/2, lookup/1, start_link/0]).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-export_type([maybe_pid/0]).

-type maybe_pid() :: chkrec_keeper:maybe(pid()) | down.
%% Optionally present pid.

-type sources() :: dict:dict(term(), maybe_pid()).
%% Mapping of identifiers for useful workers to pids of backup keepers.

-type refs() :: dict:dict(reference(), term()).
%% Mapping of references for monitored backup keepers to identifiers for
%% useful workers.

-record(keeper_mon, {
    sources = dict:new() :: sources()
  , refs = dict:new() :: refs()
}).

-type keeper_mon() :: #keeper_mon{
    sources :: sources()
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

%% @doc Creates a new backup keeper under the supervisor of the
%%      `chkrec_keeper_sup' process.
new(Source) ->
    Result = supervisor:start_child(?KEEPER_SUP, [Source])
  , case Result of
        {ok, Keeper} -> update(Source, Keeper)
      ; _Else -> pass
    end

  , Result
.

-spec update(Source :: term(), Keeper :: pid()) -> ok.

%% @doc Changes the pid associated with the specified source.
update(Source, Keeper) when is_pid(Keeper) ->
    gen_server:cast(?MODULE, {update, Source, Keeper})
.

-spec lookup(Source :: term()) ->
    {ok, maybe_pid()}
  | {error, Reason :: term()}
.

%% @doc Returns the pid associated with the specified source.
lookup(Source) ->
    gen_server:call(?MODULE, {lookup, Source}, ?TIMEOUT)
.

%%--------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()}
  | {error, Reason :: term()}
.

%% @doc Convenience function to create a backup keeper monitor as part
%%      of a supervisor hierarchy, registered locally as
%%      `chkrec_keeper_mon'.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------

-spec init(Args :: []) ->
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

%% @doc Called by a `gen_server' to handle a synchronous message.
handle_call({lookup, Source}, _From, State) ->
    Sources = State#keeper_mon.sources
  , MaybeKeeper = case dict:find(Source, Sources) of
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

%% @doc Called by a `gen_server' to handle an asynchronous message.
handle_cast(
    {update, Source, Keeper}
  , State0 = #keeper_mon{sources = Sources0, refs = Refs0}
) ->
    % Check if already exists pid that is mapped by `Source'.
    case dict:find(Source, Sources0) of
        {ok, {value, OldPid}} ->
            % Find the reference that maps to `Source'.
            {ok, OldRef} = dict:fold(
                find_key_with_value(Source)
              , {error, not_found}
              , Refs0)

            % Turn off monitoring of `OldPid',
            % and clear `DOWN' message if exists.
          , erlang:demonitor(OldRef, [flush])
            % Terminate `OldPid' "normally," so as to avoid restart.
          , erlang:exit(OldPid, {shutdown, for_update})

      ; _Else -> pass
    end

  , Sources1 = dict:store(Source, Keeper, Sources0)

  , MonitorRef = erlang:monitor(process, Keeper)
  , Refs1 = dict:store(MonitorRef, Source, Refs0)

  , State1 = State0#keeper_mon{sources=Sources1, refs=Refs1}
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

%% @doc Called by a `gen_server' to handle a message other than a
%%      synchronous or asynchronous message.
handle_info(
    {'DOWN', MonitorRef, process, _Keeper, Reason}
  , State0 = #keeper_mon{sources = Sources0, refs = Refs0}
) ->
    % Assumed that `MonitorRef' is present in the mapping of references.
    Source = dict:fetch(MonitorRef, Refs0)
  , Refs1 = dict:erase(MonitorRef, Refs0)

    % Assumed that `Source' is present in the sources, and is
    % associated with `{value, _Keeper}'.
  , Sources1 = case is_normal_reason(Reason) of
        % Removes the entry in the case where the process will NOT be
        % restarted by its supervisor.
        true -> dict:erase(Source, Sources0)

      ; false -> dict:store(Source, down, Sources0)
    end

  , State1 = State0#keeper_mon{sources=Sources1, refs=Refs1}
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

%%====================================================================
%% private functions
%%====================================================================

-spec find_key_with_value(X :: term()) ->
    fun ((Key :: term(), Value :: term(), AccIn) -> AccOut)
  when
    AccIn :: {ok, Key :: term()} | {error, Reason}
  , AccOut :: {ok, Key :: term()} | {error, Reason}
  , Reason :: not_found | multiple_keys
.

%% @doc Returns a function that yields the key associated with the
%%      specified value. Intended to be used with `dict:fold/3'.
find_key_with_value(X) ->
    fun (Key, Value, Result) when Value =:= X ->
        case Result of
            {error, not_found} -> {ok, Key}
          ; _Else -> {error, multiple_keys}
        end

      ; (_Key, _Value, Result) -> Result
    end
.

-spec is_normal_reason(Reason :: term()) -> boolean().

%% @doc Returns `true' if the specified reason is indicative of a
%%      "normal" exit, and `false' otherwise.
is_normal_reason(normal) -> true;
is_normal_reason(shutdown) -> true;
is_normal_reason({shutdown, _}) -> true;
is_normal_reason(_Reason) -> false.

%%--------------------------------------------------------------------
