%% @author Max Hirschhorn <maxh@caltech.edu>

%% @doc TODO
-module(chkrec_server).
-behaviour(gen_server).
-compile(no_auto_import).

-define(TIMER_FREQUENCY, 60000).
-define(RECEIVE_THRESHOLD, 100).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
  , code_change/3
]).

-type option() ::
    {senders, list()}
  | {timer_freq, pos_integer() | infinity}
  | {receive_thresh, pos_integer() | infinity}
.

-record(worker, {
    module :: atom()
  , context :: term()
  , senders = [] :: list()
  , timer_freq = ?TIMER_FREQUENCY :: pos_integer() | infinity
  , timer_ref :: reference()
  , receive_thresh = ?RECEIVE_THRESHOLD :: pos_integer() | infinity
  , receive_count = 0 :: non_neg_integer()
}).

-type worker() :: #worker{
    module :: atom()
  , context :: term()
  , senders :: list()
  , timer_freq :: pos_integer() | infinity
  , timer_ref :: reference() | undefined
  , receive_thresh :: pos_integer() | infinity
  , receive_count :: non_neg_integer()
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
  , State0 :: term()
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
  , State0 :: term()
  , Extra :: term()
) ->
    {ok, State1 :: term()}
  | {error, Reason :: term()}
.

%%--------------------------------------------------------------------

-spec init(X :: {Mod :: atom(), Args :: term(), Options :: [option()]}) ->
    {ok, State :: worker()}
  | {ok, State :: worker(), timeout() | hibernate}
  | {stop, Reason :: term()}
  | ignore
.

%% @doc Initializes the internal state of the useful worker.
init({Mod, Args, Options}) ->
    case Mod:init(Args) of
        {ok, Context} ->
            State0 = #worker{module=Mod, context=Context}
          , case init(Options, State0) of
                {ok, State1} -> {ok, State1}
              ; {error, Reason} -> {stop, Reason}
            end

      ; {ok, Context, Timeout} ->
            State0 = #worker{module=Mod, context=Context}
          , case init(Options, State0) of
                {ok, State1} -> {ok, State1, Timeout}
              ; {error, Reason} -> {stop, Reason}
            end

      ; Else -> Else
    end
.

%%--------------------------------------------------------------------

-spec handle_call(
    Request :: term()
  , From :: {pid(), Tag :: term()}
  , State0 :: worker()
) ->
    {reply, Reply :: term(), State1 :: worker()}
  | {reply, Reply :: term(), State1 :: worker(), timeout() | hibernate}
  | {noreply, State1 :: worker()}
  | {noreply, State1 :: worker(), timeout() | hibernate}
  | {stop, Reason :: term(), Reply :: term(), State1 :: worker()}
  | {stop, Reason :: term(), State1 :: worker()}
.

%% @doc Called by a `gen_server' to handle a synchronous message.
handle_call(
    Request
  , From
  , State0 = #worker{module = Mod, context = Context0}
) ->
    case Mod:handle_call(Request, From, Context0) of
        {reply, Reply, Context1} ->
            State1 = State0#worker{context=Context1}
          , {reply, Reply, State1}

      ; {reply, Reply, Context1, Timeout} ->
            State1 = State0#worker{context=Context1}
          , {reply, Reply, State1, Timeout}

      ; {noreply, Context1} ->
            State1 = State0#worker{context=Context1}
          , {noreply, State1}

      ; {noreply, Context1, Timeout} ->
            State1 = State0#worker{context=Context1}
          , {noreply, State1, Timeout}

      ; {stop, Reason, Reply, Context1} ->
            State1 = State0#worker{context=Context1}
          , {stop, Reason, Reply, State1}

      ; {stop, Reason, Context1} ->
            State1 = State0#worker{context=Context1}
          , {stop, Reason, State1}
    end
.

%%--------------------------------------------------------------------

-spec handle_cast(
    Request :: term()
  , State0 :: worker()
) ->
    {noreply, State1 :: worker()}
  | {noreply, State1 :: worker(), timeout() | hibernate}
  | {stop, Reason :: term(), State1 :: worker()}
.

%% @doc Called by a `gen_server' to handle an asynchronous message.
handle_cast(Request, State0 = #worker{module = Mod, context = Context0}) ->
    case Mod:handle_cast(Request, Context0) of
        {noreply, Context1} ->
            State1 = State0#worker{context=Context1}
          , {noreply, State1}

      ; {noreply, Context1, Timeout} ->
            State1 = State0#worker{context=Context1}
          , {noreply, State1, Timeout}

      ; {stop, Reason, Context1} ->
            State1 = State0#worker{context=Context1}
          , {stop, Reason, State1}
    end
.

%%--------------------------------------------------------------------

-spec handle_info(
    Info :: timeout | term()
  , State0 :: worker()
) ->
    {noreply, State1 :: worker()}
  | {noreply, State1 :: worker(), timeout() | hibernate}
  | {stop, Reason :: term(), State1 :: worker()}
.

%% @doc Called by a `gen_server' to handle a message other than a
%%      synchronous or asynchronous message.
handle_info(Info, State0 = #worker{module = Mod, context = Context0}) ->
    case Mod:handle_info(Info, Context0) of
        {noreply, Context1} ->
            State1 = State0#worker{context=Context1}
          , {noreply, State1}

      ; {noreply, Context1, Timeout} ->
            State1 = State0#worker{context=Context1}
          , {noreply, State1, Timeout}

      ; {stop, Reason, Context1} ->
            State1 = State0#worker{context=Context1}
          , {stop, Reason, State1}
    end
.

%%--------------------------------------------------------------------

-spec terminate(
    Reason :: normal | shutdown | {shutdown, term()} | term()
  , State :: worker()
) ->
    no_return()
.

%% @doc Called by a `gen_server' when it is about to terminate.
%%      Defer to the callback module.
terminate(Reason, #worker{module = Mod, context = Context}) ->
    Mod:terminate(Reason, Context)
.

-spec code_change(
    OldVsn :: term()
  , State0 :: worker()
  , Extra :: term()
) ->
    {ok, State1 :: worker()}
  | {error, Reason :: term()}
.

%% @doc Called by a `gen_server' when it should update its internal
%%      state during a release upgrade or downgrade.
%%      Defer to the callback module.
code_change(
    OldVsn
  , State0 = #worker{module = Mod, context = Context0}
  , Extra
) ->
    case Mod:code_change(OldVsn, Context0, Extra) of
        {ok, Context1} ->
            State1 = State0#worker{context=Context1}
          , {ok, State1}

      ; Else -> Else
    end
.

%%====================================================================
%% private functions
%%====================================================================

%% @doc Initializes the internal state of the useful worker.
-spec init(Options :: [option()], State0 :: worker()) ->
    {ok, State1 :: worker()}
  | {error, Reason :: term()}
.

init([], State) ->
    {ok, State}
;

init([{senders, Senders} | Options], State0) ->
    State1 = State0#worker{senders=Senders}
  , init(Options, State1)
;

init([{timer_freq, TimerFreq} | Options], State0) ->
    % TODO validate type and value of `TimerFreq'
    State1 = State0#worker{timer_freq=TimerFreq}
  , init(Options, State1)
;

init([{receive_thresh, ReceiveThresh} | Options], State0) ->
    % TODO validate type and value of `ReceiveThresh'
    State1 = State0#worker{receive_thresh=ReceiveThresh}
  , init(Options, State1)
;

init([Term | _Options], _State) ->
    {error, {badarg, Term}}
.

%%--------------------------------------------------------------------
