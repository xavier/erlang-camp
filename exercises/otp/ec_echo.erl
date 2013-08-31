-module(ec_echo).
-export([start/0]).

%% Usage from remote node:
%% lists:foreach(fun(Node) -> {ec_echo, Node} ! {self(), echo_me_please} end, nodes()).
start() ->
  spawn(fun() ->
    register(ec_echo, self()),
    loop()
  end).

loop() ->
  receive
    {From, stop} ->
      From ! stopped,
      ok;
    {From, Msg} ->
      From ! Msg,
      loop()
  end.