-module(minidote_database).
-export([new/0, is_key/2, state/2, value/2, update_state/3]).
-export_type([database/0]).


-type key() :: minidote:key().
-type crdt() :: antidote_crdt:crdt().
-type database() :: #{key() := crdt()}.


-spec new() -> database().

new() -> #{}.

-spec state(Database :: database(),
    Key :: key()) ->
    {ok, antidote_crdt:crdt()}
  | {error, any()}.

state(Database, Key) ->
  case maps:is_key(Key, Database) of
    true -> 
      {ok, maps:get(Key, Database)};
    false -> 
      {_, Type, _} = Key,
      {ok, antidote_crdt:new(Type)}
  end.

-spec is_key(key(), database()) -> boolean().

is_key(Key, Database) ->
  maps:is_key(Key, Database).

-spec value(Database :: database(),
    Key :: key()) ->
    {ok, antidote_crdt:value()} 
  | {error, any()}.

value(Database, Key = {_, Type, _}) ->
  {ok, State} = state(Database, Key),
  antidote_crdt:value(Type, State).

-spec update_state(Database :: database(),
    Key :: key(),
    State :: antidote_crdt:crdt()) ->
  {ok, database()}.

update_state(Database, Key, State) ->
    % No matter if the key does not exist, #{a => b} will create it.
    {ok, Database#{Key => State}}.
     



