-module(server).
-export([start/0, lookup/1, store/2]).

%%% API

start() ->
  spawn(fun() -> init() end).

% Encapsulation: we map functions to messages
store(Key, Value) ->
  server ! {store, {Key, Value}}.

lookup(Key) ->
  server ! {lookup, {Key, self()}},
  receive
    Msg -> {ok, Msg}
  end.

%%% Internal Functions

% register(name, Pid) gives a name to the process (unique per node)
init() ->
  register(server, self()),
  io:format("starting~n"),
  loop([]).

% receive pulls a message from the mailbox using a pattern
loop(State) ->
    receive
      {lookup, {Key, From}} ->
        From ! proplists:get_value(Key, State),
        loop(State);
      {store, {Key, Value}} ->
        io:format("storing ~p~n", [Key]),
        loop([{Key, Value}|State])
    end.
