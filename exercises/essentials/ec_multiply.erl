%%% @author Xavier
%%% @doc contains mult func
%%% @copyright 2013 ACME Inc.

-module(ec_multiply).
-export([mult/3]).

%% @doc adds two numbers together
-spec(mult(number(), number(), number()) -> number()).
mult(A, B, C) ->
  A * B * C.