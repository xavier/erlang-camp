%%% @author Xavier
%%% @doc contains print_each, twomult
%%% @copyright 2013 ACME Inc.

-module(ec_lists).
-export([print_each/1, twomult/1, twomult_comp/1, yourmap/2]).

%% Usage: ec_lists:print_each([a, b, c])
print_each(List) ->
  print_each(1, List).

print_each(_, []) ->
  ok;
print_each(Index, [Head|Tail]) ->
  io:format("~p is ~p~n", [Index, Head]),
  print_each(Index+1, Tail).

-spec(twomult(list()) -> list()).
twomult([]) ->
  [];
twomult([H|T]) ->
  [H*2|twomult(T)].

% Using list comprehension
twomult_comp(L) ->
  [X * 2 || X <- L].

yourmap(_, []) ->
  [];
yourmap(Fun, [_H|_T]) ->
  [Fun(_H)|yourmap(Fun, _T)].
