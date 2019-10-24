-module(minidote_broadcast).
-export([start_link/1, broadcast/2]).

start_link(RespondTo) ->
  {ok, LinkLayer} = link_layer_distr_erl:start_link(minidote),
  causal_broadcast:start_link(LinkLayer, RespondTo).

broadcast(B, Msg) ->
  causal_broadcast:broadcast(B, Msg).

