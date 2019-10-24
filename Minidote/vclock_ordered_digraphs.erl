-module(vclock_ordered_digraphs).
-export([new/0, add/3, from_list/1, sorted_list/1]).

-type digraph() :: digraph:graph().
-type clock() :: vector_clock:vector_clock().


-spec new() -> digraph().

new() -> digraph:new().

-spec add(digraph(), 
          Label :: any(),
          clock()) ->
  digraph().

add(Digraph, Label, Clock) ->
  Size = digraph:no_vertices(Digraph),
  Name = Size + 1,
  NewVertex = digraph:add_vertex(Digraph, Name, {Label, Clock}),
  Vertices = [ digraph:vertex(Digraph, V) || V <- digraph:vertices(Digraph) ],
  Edges = digraph:edges(Digraph),
  %io:format("~w~n", ["LOGGERTEST"]),
  %io:format("~w~n", [io_lib:write(Vertices)]),
  _NewEdges = lists:foldl(fun({OldVertex, {_, OldClock}}, EdgesAcc) ->
      if 
        Clock == ignore orelse OldClock == ignore -> EdgesAcc;
        true -> 
          case vector_clock:leq(Clock, OldClock) of
            true -> NewEdge = digraph:add_edge(Digraph, NewVertex, OldVertex),
                    [NewEdge | EdgesAcc];
            false -> case vector_clock:leq(OldClock, Clock) of 
                       true -> NewEdge = digraph:add_edge(Digraph, OldVertex, NewVertex),
                               [NewEdge | EdgesAcc];
                       false -> EdgesAcc
                     end
          end
      end
                          end,
    Edges,
    Vertices),    
  Digraph.

-spec from_list([{digraph:label(), clock()}]) -> digraph().

from_list(List) -> lists:foldl(fun({Label, Clock}, DigraphAcc)-> 
                           add(DigraphAcc, Label, Clock)
                         end, 
                         new(), 
                         List).

-spec sorted_list(digraph()) -> [{digraph:vertex(), digraph:label()}].

sorted_list(Digraph) -> 
  SortedVertices = digraph_utils:topsort(Digraph),
  [ digraph:vertex(Digraph, Vertex) || Vertex <- SortedVertices ].



  

