# Erlang Essentials

## Module Definition

`-spec` is an optional type signature that provide hints to the compiler, static analyzers and code documentation tools.

**atoms** start with `[a-z]` unless it's enclosed in single quotes, e.g. `'+'`.

## Pattern Matching

`1 = 1` is not an assignment, it's **pattern matching**.

`1 == 1` is a comparison and evaluates to `true`.

`f(_X)` the underscore means we don't care about the variable.

### Guards

    not_less_than_2(N) when N >= 2 ->
      "good";
    not_less_than_2(N)
      "bad".