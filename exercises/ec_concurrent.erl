%%% @author Xavier
%%% @doc contains bf_seq
%%% @copyright 2013 ACME Inc.

-module(ec_concurrent).
-export([bf_seq/1]).

% Prints a sequence backwards and forwards simultaneously
-spec(bf_seq(list()) -> ok).
bf_seq(L) ->
  spawn(fun() -> print_list(lists:reverse(L)) end),
  print_list(L).

print_list([]) ->
  ok;
print_list([Head|Tail]) ->
  io:format("~p,", [Head]),
  print_list(Tail).
