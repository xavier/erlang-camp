%%% @author Xavier
%%% @doc contains sum_seq
%%% @copyright 2013 ACME Inc.

-module(ec_case).
-export([is_a_list/1]).

is_a_list(L) ->
  case L of
    [] ->
      true;
    [_|_] ->
      true;
    _ ->
      false
  end.
