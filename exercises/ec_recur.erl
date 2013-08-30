%%% @author Xavier
%%% @doc contains sum_seq
%%% @copyright 2013 ACME Inc.

-module(ec_recur).
-export([sum_seq/1, sum_seq_tail/1, sum_seq_tail/2]).

sum_seq(0) ->
  0;
sum_seq(N) when N > 0 ->
  N + sum_seq(N-1).

sum_seq_tail(N) when N > 0 ->
  sum_seq_tail(N, 0).

sum_seq_tail(0, Acc) ->
  Acc;
sum_seq_tail(N, Acc) ->
  sum_seq_tail(N-1, Acc+N).

%% ;)
sum_seq_euler(N) ->
  (N * (N+1)) div 2.