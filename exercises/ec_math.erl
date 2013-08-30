%%% @author Xavier
%%% @doc contains op
%%% @copyright 2013 ACME Inc.

-module(ec_math).
-export([op/3]).

-spec(op(atom(), number(), number()) -> number()).
op('+', A, B) ->
  op(add, A, B);
op(add, A, B) ->
  A + B;
op(sub, A, B) when B < A ->
  A - B;
op(sub, _, _) ->
  error.
