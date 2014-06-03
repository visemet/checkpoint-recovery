%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO
-module(chkrec_worker).
-behaviour(gen_server).
-compile(no_auto_import).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-record(worker, {
    module :: atom()
  , context :: term()
}).

-type worker() :: #worker{
    module :: atom()
  , context :: term()
}.
%% Internal useful worker state.

%%====================================================================
%% API
%%====================================================================

-callback init(
    Args :: term()
) ->
    {ok, State :: term()}
  | {ok, State :: term(), timeout() | hibernate}
  | {stop, Reason :: term()}
  | ignore
.

-callback handle_call(
    Request :: term()
  , From :: {pid(), Tag :: term()}
  , State0 :: term()
) ->
    {reply, Reply :: term(), State1 :: term()}
  | {reply, Reply :: term(), State1 :: term(), timeout() | hibernate}
  | {noreply, State1 :: term()}
  | {noreply, State1 :: term(), timeout() | hibernate}
  | {stop, Reason :: term(), Reply :: term(), State1 :: term()}
  | {stop, Reason :: term(), State1 :: term()}
.

-callback handle_cast(
    Request :: term()
  , State0 :: term()
) ->
    {noreply, State1 :: term()}
  | {noreply, State1 :: term(), timeout() | hibernate}
  | {stop, Reason :: term(), State1 :: term()}
.

-callback handle_info(
    Info :: timeout | term()
  , State :: term()
) ->
    {noreply, State1 :: term()}
  | {noreply, State1 :: term(), timeout() | hibernate}
  | {stop, Reason :: term(), State1 :: term()}
.

-callback terminate(
    Reason :: normal | shutdown | {shutdown, term()} | term()
  , State :: term()
) ->
    no_return()
.

-callback code_change(
    OldVsn :: term() | {down, term()}
  , State :: term()
  , Extra :: term()
) ->
    {ok, State1 :: term()}
  | {error, Reason :: term()}
.

%%--------------------------------------------------------------------

-spec init(X :: {Mod :: atom(), Args :: list(), Options :: list()}) ->
    {ok, State :: worker()}
  | {stop, Reason :: term()}
.

%% @doc Initializes the internal state of the useful worker.
init({Mod, _Args, _Options}) ->
    % TODO call `Mod:init/1', etc.
    State = #worker{module=Mod}
  , {ok, State}
.

%%--------------------------------------------------------------------

-spec handle_call(
    Request :: term()
  , From :: {pid(), Tag :: term()}
  , State0 :: worker()
) ->
    {reply, Reply :: term(), State1 :: worker()}
.

%% @doc Called by a `gen_server' to handle a synchronous message.
handle_call(_Request, _From, State) ->
    {reply, ok, State}
.

%%--------------------------------------------------------------------

-spec handle_cast(
    Request :: term()
  , State0 :: worker()
) ->
    {noreply, State1 :: worker()}
.

%% @doc Called by a `gen_server' to handle an asynchronous message.
handle_cast(_Request, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec handle_info(
    Info :: timeout | term()
  , State0 :: worker()
) ->
    {noreply, State1 :: worker()}
.

%% @doc Called by a `gen_server' to handle a message other than a
%%      synchronous or asynchronous message.
handle_info(_Info, State) ->
    {noreply, State}
.

%%--------------------------------------------------------------------

-spec terminate(Reason :: term(), State :: worker()) -> ok.

%% @doc Called by a `gen_server' when it is about to terminate.
%%      Nothing to clean up though.
terminate(_Reason, _State) -> ok.

-spec code_change(
    OldVsn :: term()
  , State0 :: worker()
  , Extra :: term()
) ->
    {ok, State1 :: worker()}
.

%% @doc Called by a `gen_server' when it should update its internal
%%      state during a release upgrade or downgrade.
%%      Nothing to change though.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
