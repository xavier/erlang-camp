-module(ec_echo).
-export([start/0]).

%% Usage from remote node:
%% lists:foreach(fun(Node) -> {ec_echo, Node} ! {self(), echo_me_please} end, nodes()).
start() ->
  spawn(fun() ->
    register(ec_echo, self()),
    receive {From, Msg} -> From ! Msg end
  end).
