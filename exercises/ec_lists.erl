%%% @author Xavier
%%% @doc contains print_each
%%% @copyright 2013 ACME Inc.

-module(ec_lists).
-export([print_each/1]).

%% Usage: ec_lists:print_each([a, b, c])
print_each(List) ->
  print_each(1, List).

print_each(_, []) ->
  ok;
print_each(Index, [Head|Tail]) ->
  io:format("~p is ~p~n", [Index, Head]),
  print_each(Index+1, Tail).
