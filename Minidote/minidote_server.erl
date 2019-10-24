-module(minidote_server).
-behaviour(gen_server).

% API
-export([start_link/0]).

% Specific part for gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


%-----------%
%   Types   %
%-----------%

% request, update_request read_request, effect and accompanying  types
-include("request.hrl").

-type vector_clock() :: vector_clock:clock().
-type clock() :: vector_clock().
-type waiting_requests() :: vclock_ordered_requests:vclok_ordered_requests().
-type post_process_data() :: [{update_request(), effects_for_keys()}].
-type minidote_database() :: minidote_database:database().
-type read_reply_payload() :: minidote:read_reply_payload().
-type replies_for_interfaces() :: [{read_reply_payload(), {pid(), any()}}].
-type effects_for_keys() :: [{key(), [effects()]}].
% -type processed_updates() :: [{update_request(), clock_after_update()}].
% -type clock_after_update() :: vector_clock().


%-------------%
%   Records   %
%-------------%

-record(state, {
  broadcast_id :: pid(),
  clock :: clock(),
  database :: minidote_database(),
  waiting :: waiting_requests()
}
).


%---------%
%   API   %
%---------%

% Interface is the process that sits
% between the minidote server process and the clients.
start_link() ->
  gen_server:start_link({local, minidote_server}, ?MODULE, [], []).


%---------------------------%
%   gen_server callbacks    %
%---------------------------%

init(_) ->
  {ok, BroadcastId} = minidote_broadcast:start_link(self()),
  Database = minidote_database:new(),
  Clock = vector_clock:new(),
  Waiting = vclock_ordered_requests:new(),
  {ok, #state{broadcast_id = BroadcastId, clock = Clock, database = Database, waiting = Waiting}}.

handle_call({update, Updates, ClockDependency}, From, State) ->
  logger:log(notice, "Call: ~s", [io_lib:write({update, Updates, ClockDependency})]),
  Request = #request{
               type = update,
               data = Updates,
               clock_dependency = ClockDependency,
               from = From
  },
  {ShouldProceed, NewWaiting} = putToWaitingIfUnservable(Request,
    State#state.clock,
    State#state.waiting),
  case ShouldProceed of
    true ->
      {NewDatabase, EffectsForKeys} = apply_update(Request, State#state.database, State#state.clock),
      minidote_broadcast:broadcast(State#state.broadcast_id, {EffectsForKeys, node()}),
      % TODO will we have problems with node() after process restart?
      NewClock = vector_clock:increment(State#state.clock, node()),
      {reply, {update_ok, NewClock}, State#state{database = NewDatabase, clock = NewClock}};
    false ->
      {noreply, State#state{waiting = NewWaiting}}
  end;

% Read request from the Interface
handle_call({read, Objects, ClockDependency}, From, State) ->
  logger:log(notice, "Call: ~s", [io_lib:write({read, Objects, ClockDependency})]),
  Request = #request{
               type = read,
               data = Objects,
               clock_dependency = ClockDependency,
               from = From
  },
  {ShouldProceed, NewWaiting} = putToWaitingIfUnservable(Request,
    State#state.clock,
    State#state.waiting),
  % If ShouldProceed, the state is only read, if not, a new request is put to Waiting
  case ShouldProceed of
    true ->
      logger:log(notice, "Request is servable, replying."),
      {reply, {read_ok, reply_data(Request, State#state.database), State#state.clock}, State};
    false ->
      logger:log(notice, "Request is unservable, noreplying."),
      {noreply, State#state{waiting = NewWaiting}}
  end.

% Delivery of an update from the broadcast process
% {broadcast, Effects :: [antidote_crdt:effect()], ClockDependency::vector_clock()} is
% ClockDependency here is not the dependency of the original update request from the interface,
% but the value of the vector clock of the broadcasting process at the timepoint of the broadcasting.
% the server-level message
% bc_delivery always happens in correct order thanks to the PCB, so,
% apparently we need only to apply the effects.
% There is no interdependency between update and effect application:
% If the first can be applied, it should be. The second is appliable as soon as it is delivered
% thanks to the causal broadcast.
handle_info({deliver, {Effects, SenderPid}}, State) ->
  logger:log(notice,"Delivery: ~s", [io_lib:write({Effects, SenderPid})]),
  case SenderPid =:= node() of
    true ->
      logger:log(notice, "Message originates from this node, noreplying"),
      {noreply, State};

    false ->
      logger:log(notice, "Message originates from another node, applying effects."),
      NewDatabase =
        apply_effects(
          Effects,
          State#state.database
        ),
      NewClock = vector_clock:increment(State#state.clock, SenderPid),
      logger:log(notice, "Clock after applying updates: ~s", [io_lib:write(NewClock)]),

      % Try to apply the waiting updates
      {NewestDatabase, NewWaiting, PostProcessData} =
        apply_waiting_updates(State#state.waiting, NewClock, NewDatabase),
      NewestClock = lists:foldl(fun({ProcessedUpdate, EffectsForKeys}, ClockAcc) ->
        {update, _, _, From} = ProcessedUpdate,
        minidote_broadcast:broadcast(State#state.broadcast_id, {broadcast, EffectsForKeys, node()}),
        ClockAfterUpdate = vector_clock:increment(ClockAcc, node()),
        gen_server:reply(From, {update_ok, ClockAfterUpdate}),
        ClockAfterUpdate end,
        NewClock,
        PostProcessData),
      logger:log(notice, "Waiting updates processed"),

      % Try to apply the waiting reads
      {NewestWaiting, RepliesForInterfaces} = process_waiting_reads(NewWaiting, State#state.clock, State#state.database),
      lists:foreach(fun({ReplyData, From}) ->
        gen_server:reply(From, {read_ok, ReplyData, NewestClock}) end,
        RepliesForInterfaces),
      logger:log(notice, "Waiting reads processed, noreplying"),

      {noreply, State#state{database = NewestDatabase, clock = NewestClock, waiting = NewestWaiting}}
  end.


handle_cast(_,_) -> throw("Not implemented").


%-------------------------------%
%     Internal Functions        %
%-------------------------------%

-spec putToWaitingIfUnservable(
    request(),
    vector_clock(),
    waiting_requests()
  ) ->  {ShouldProceed :: boolean(), Waiting :: waiting_requests()}.

putToWaitingIfUnservable(Request, Clock, Waiting) ->
  ClockDependency = Request#request.clock_dependency,
  logger:log(notice, "__putToWaitingIfUnservable__~nClock: ~s~nClockDependency: ~s",[io_lib:write(Clock), io_lib:write(ClockDependency)]),
  case ClockDependency /= ignore andalso not vector_clock:leq(ClockDependency, Clock) of
    true ->
      logger:log(notice, "{false, NewWaiting}"),
      NewWaiting = vclock_ordered_requests:add(Request,
      Waiting),
      {false, NewWaiting};
    false ->
      logger:log(notice, "{true, Waiting}"),
      {true, Waiting}
  end.

% Assumption: All updates are correct, e.g. only valid update operations and only supported types.
-spec apply_update(
    update_request(),
    minidote_database(),
    vector_clock()
  ) -> {minidote_database(), effects_for_keys()}.

apply_update(Request, Database, Clock) ->
  #request{
    type = update,
    data = Data,
    clock_dependency = ClockDependency
  } = Request,
  case ClockDependency == ignore orelse vector_clock:leq(ClockDependency, Clock) of
    true ->
      {NewDatabase, ReverseOrderedEffects} = lists:foldl(fun({Key = {_, Type, _}, Operation, Arguments}, {DatabaseAcc, EffectsAcc} ) ->
        CurrentCRDTState = current_or_new_state(DatabaseAcc, Key),
        {ok, Effect} = antidote_crdt:downstream(Type, {Operation, Arguments}, CurrentCRDTState),
        {ok, NewCRDTState} = antidote_crdt:update(Type, Effect, CurrentCRDTState),
        {ok, NewDatabase} = update_database(DatabaseAcc, Key, NewCRDTState),
        {NewDatabase, [{Key, Effect} | EffectsAcc]}
        end,
        {Database, []},
        Data),
      Effects = lists:reverse(ReverseOrderedEffects),
      {NewDatabase, Effects};
    false -> {Database, []}
  end.

% The effects_for_keys/0 are ment to originate from a single update_request,
% so the clock is inceremented only once.
-spec apply_effects(effects_for_keys(),
                    minidote_database()) ->
  minidote_database().

apply_effects(EffectsForKeys, Database)  ->
      NewDatabase = lists:foldl(fun({Key, Effect}, DatabaseAcc) ->
        {_, Type, _} = Key,
        CurrentCRDTState = current_or_new_state(DatabaseAcc, Key),
        {ok, NewCRDTState} = antidote_crdt:update(Type, Effect, CurrentCRDTState),
        {ok, NewDatabase} = update_database(DatabaseAcc, Key, NewCRDTState),
        NewDatabase end,
        Database,
        EffectsForKeys),
      NewDatabase.

-spec current_state(Database :: minidote_database:minidote_database(),
    Key :: key()) ->
    {ok, antidote_crdt:crdt()}
  | {error, any()}.

current_state(Database, Key) ->
  minidote_database:state(Database, Key).

-spec current_or_new_state(minidote_database(), key())
  -> antidote_crdt:state().

current_or_new_state(Database, Key) ->
  {ok, SomeState} = current_state(Database, Key),
  SomeState.

-spec update_database(Database :: minidote_database(),
    Key :: key(),
    NewCRDTState :: antidote_crdt:state()) ->
  {ok, minidote_database()}.

update_database(Database, Key, NewCRDTState) ->
  minidote_database:update_state(Database, Key, NewCRDTState).

-spec reply_data(Request :: read_request(),
    Database :: minidote_database()) ->
  read_reply_payload().

reply_data(Request, Database) ->
  #request{
    type = read,
    data = Data
  } = Request,
  lists:map(fun(Key) -> {Key, minidote_database:value(Database, Key)} end, Data).

-spec apply_waiting_updates(Waiting :: waiting_requests(),
    Clock :: vector_clock(),
    Database :: minidote_database()) ->
  {minidote_database(),
    waiting_requests(),
    post_process_data()}.

apply_waiting_updates(Waiting, Clock, Database) ->
  WaitingUpdates = vclock_ordered_requests:filter_by_request_type(update, Waiting),
  {ResultDB, _, ResultWaiting, ResultPPD} = vclock_ordered_requests:foldl(fun(Update,
      {DatabaseAcc, ClockAcc, WaitingAcc, PostProcessDataAcc}) ->
    #request{ type = update, clock_dependency = ClockDependency} = Update,
    case ClockDependency == ignore orelse vector_clock:leq(ClockDependency, ClockAcc) of
      true ->
        {NewDatabase, EffectsForKeys} = apply_update(Update,
          DatabaseAcc,
          ClockAcc),
        NewClock = vector_clock:increment(ClockAcc, node()),
        NewWaiting = vclock_ordered_requests:remove(Update, WaitingAcc),
        NewPostProcessData = [{Update, EffectsForKeys} | PostProcessDataAcc],
        {NewDatabase, NewClock, NewWaiting, NewPostProcessData};
      false ->
        % if ClockDependency not met, it is also the case for all remaining
        % updates (the requests are ordered), so stop.
        {DatabaseAcc, ClockAcc, WaitingAcc, PostProcessDataAcc}
    end
                                end,
    {Database, Clock, Waiting, []},
    WaitingUpdates),
  {ResultDB, ResultWaiting, ResultPPD}.

-spec process_waiting_reads(Waiting :: waiting_requests(),
    Clock :: vector_clock(),
    Database :: minidote_database()) ->
  {waiting_requests(), replies_for_interfaces()}.

process_waiting_reads(Waiting, Clock, Database) ->
  WaitingReads = vclock_ordered_requests:filter_by_request_type(read, Waiting),
  vclock_ordered_requests:foldl(fun(Request, {WaitingAcc, ValuesForReadsAcc}) ->
    #request{type = read, clock_dependency = ClockDependency, from = From} = Request,
    case ClockDependency == ignore
      orelse vector_clock:leq(ClockDependency, Clock) of
      true ->
        ReplyData = reply_data(Request, Database),
        NewWaiting = vclock_ordered_requests:remove(Request, WaitingAcc),
        {NewWaiting, [{ReplyData, From} | ValuesForReadsAcc]};
      false ->
        {WaitingAcc, ValuesForReadsAcc}
    end
                                end,
    {Waiting, []},
    WaitingReads).


