# Erlang Essentials

## Module Definition

`-spec` is an optional type signature that provide hints to the compiler, static analyzers and code documentation tools.

**atoms** start with `[a-z]` unless it's enclosed in single quotes, e.g. `'+'`.

## Pattern Matching

`1 = 1` is not an assignment, it's **pattern matching**.

`1 == 1` is a comparison and evaluates to `true`.

`f(_X)` the underscore means we don't care about the variable.

Order is significant (but not for performance reasons).

### Guards

    not_less_than_2(N) when N >= 2 ->
      "good";
    not_less_than_2(N)
      "bad".

### Recursion

    run_baby_run(0) ->
      ok;
    run_baby_run(N) ->
      run_baby_run(N-1).

This function is **tail recursive**: the last thing we do is to call a function, the VM will  not save the stackframe.

#### Tail Recursion with Accumulator

    %% Accumulates N dots
    run_baby_run(0, Acc) ->
      Acc;
    run_baby_run(N, Acc) ->
      run_baby_run(N-1, Acc ++ ".").

## IO

    Msg = "Hello world".
    io:format("~p~n", [Msg]).

`io:format` **does not return a string**, it prints it out as a side-effect.

`~p` pretty print string
`~n` new line

## Lists

    [1, 2, 3, 4]
    [1, a, 2, b]

    Head = 1.
    Tail = [2, 3, 4].
    [Head|Tail].
    => [1, 2, 3, 4]
    [1 | [2, 3, 4]].
    => [1, 2, 3, 4]

    [Head|Tail] = [1, a, 2, b]
    Head => 1
    Tail => [a, 2, b]

## Strings

Just syntactic sugar for list of character codes.

## Anonymous Functions

    fun() -> ok end
    F = fun(X) -> [X] end
    F(abc)
    => [abc]

Higher-order function:

    lists:map(fun(N) -> N * 2 end, [1, 2, 3]).

## Case

    a_is_a(X)
      case X of
        a ->
          true;
        _ ->
          false
      end.


## Exceptions

Very uncommon and non-idiomatic in Erlang.

    try list:reverse(a) of
      List -> ok
    catch
      Error:Exception ->
        fixit()
    after
      always_do_this()
    end

* `thrown/1`: user exception
* `exit/1`:
* `error`: thrown by the VM

Read: [http://leikind.org/errors\_and\_processes/](http://leikind.org/errors_and_processes/)

## Processes

Processes are the fundamental unit of concurrency and cannot interfere with each other.

    spawn(fun() -> io:format("hello") end).

spawn returns `Pid`
