-module(vclock_ordered_requests).
-export([new/0, add/2, filter_by_request_type/2, foldl/3, remove/2]).
-export_type([vclock_ordered_requests/0]).


-include("request.hrl").

-type vclock_ordered_requests() :: [request()].

-spec new() -> vclock_ordered_requests().

new() -> [].

-spec add(request(), vclock_ordered_requests()) ->
  vclock_ordered_requests().

add(Request, Waiting) ->
  %logger:log(error, "Request: ~s", [io_lib:write(Request)]),
  ClockDependency = Request#request.clock_dependency,
  SrcList = lists:map(fun(WaitingRequest) ->
                          {_, _, ClockDep, _} = WaitingRequest,
                          {WaitingRequest, ClockDep} end,
                      Waiting),
  %logger:log(error, "SrcList: ~s", [io_lib:write(SrcList)]),
  Digraph = vclock_ordered_digraphs:from_list(SrcList),
  NewDigraph = vclock_ordered_digraphs:add(Digraph, Request, ClockDependency),
  %logger:log(error, "NewDigraph: ~s", [io_lib:write(NewDigraph)]),
  SortedList = vclock_ordered_digraphs:sorted_list(NewDigraph),
  digraph:delete(NewDigraph),
  %logger:log(error, "SortedList: ~s", [io_lib:write(SortedList)]),
  [WaitingRequest || {_, {WaitingRequest, _}} <- SortedList].

-spec filter_by_request_type('update' | 'read', vclock_ordered_requests()) ->
  vclock_ordered_requests().

filter_by_request_type(RequestType, Requests) ->
  %logger:log(error, "Requests: ~s", [io_lib:write(Requests)]),
  % debugger:start(),
  lists:filter(fun(Request) ->
    if
      Request#request.type == RequestType -> true;
      true -> false
    end
               end,
    Requests).


-spec foldl(fun((request(), A) -> A), A, vclock_ordered_requests()) -> A.

foldl(Fun, StartAcc, Requests) -> lists:foldl(Fun, StartAcc, Requests).

-spec remove(request(), vclock_ordered_requests()) -> vclock_ordered_requests().

remove(Item, Requests) -> lists:delete(Item, Requests).


