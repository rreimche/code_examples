% if  type :: 'efffect' and data :: effects(), then from means the pid() of the broadcasting process 
-record(request, {
  type :: 'update' | 'read' | 'effect',
  data :: minidote_request_data(),
  clock_dependency :: vector_clock:clock_dependency(),
  from :: {pid(), any()} 
}).

-type request() :: #request{}.
-type update_request() :: #request{type :: 'update'}.
-type read_request() :: #request{type :: 'read'}.
-type key() :: minidote:key().
-type update_request_data() :: [{key(), Op :: atom(), Args :: any()}].
-type read_request_data() :: [key()].
-type effects() :: [antidote_crdt:effect()].
-type minidote_request_data() :: update_request_data() | read_request_data() | effects().
