-module(vector_clock).
-export([new/0, increment/2, get/2, leq/2, merge/2]).
-export_type([clock/0, vector_clock/0, clock_dependency/0]).

-type vector_clock() :: [{ProcessID :: process_id(), Value :: integer()}].
-type process_id() :: any().
-type process_value() :: integer().
-type clock() :: vector_clock().
-type clock_dependency() :: vector_clock() | ignore.


% Main idea is that if there is no entry for a process,
% it means that it's vector clock equals 0.
-spec new() -> vector_clock().

new() -> [].

-spec increment(vector_clock(), process_id()) -> vector_clock().

increment([], P) -> [{P, 1}];
increment([{Pname, Clock} | T], P) ->
  if
    Pname == P -> [{Pname, Clock + 1} | T];
    true -> [{Pname, Clock}] ++ increment(T, P)
  end.


-spec get(vector_clock(), process_id()) -> process_value().

get([], _) -> 0;
get([{Pname, Clock} | T], P) ->
  case Pname of
    P -> Clock;
    _ -> get(T, P)
  end.

-spec leq(vector_clock(), vector_clock()) -> boolean().

leq([], []) -> true;
leq([], [{_, C2} | T2]) ->
  (0 =< C2) and leq([], T2);
leq([{P, C} | T], VC2) ->
  (C =< get(VC2, P)) and leq(T, VC2).

-spec merge(vector_clock(), vector_clock()) -> vector_clock().

merge([], []) -> [];
merge(VC1, []) -> VC1;
merge([], VC2) -> VC2;
merge([{P1, C1} | T1], VC2) ->
  VC2WithoutP1 = lists:delete({P1, C1}, VC2),
  [mergeSingleProcess({P1, C1}, VC2) | merge(T1, VC2WithoutP1)]. % PROBLEM: VC2 is attached to the merged result but has unmerged processes


% helper functions

mergeSingleProcess({P1, C1}, []) -> {P1, C1};
mergeSingleProcess({P1, C1}, [{P2, C2} | T2]) ->
  case P1 == P2 of
    true -> {P1, max(C1, C2)};
    false -> mergeSingleProcess({P1, C1}, T2)
  end.




