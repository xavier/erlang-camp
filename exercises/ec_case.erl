%%% @author Xavier
%%% @doc contains sum_seq
%%% @copyright 2013 ACME Inc.

-module(ec_case).
-export([is_a_list/1]).

-spec(is_a_list(list() | any()) -> boolean()).
is_a_list(L) ->
  case L of
    [] ->
      true;
    [_H|_T] ->
      true;
    _Else ->
      false
  end.
