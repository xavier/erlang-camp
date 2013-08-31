# Testing Erlang Programs

Two examples for testing Erlang programs:

* Unit testing: EUnit
* Property-based testing

## Verification and Validation

Are we building the product right?
The system should conform to its specifications.

* Dynamic: software testing (EUnit, QuickCheck, McErlang)
* Static: code inspection (Dializer)

## EUnit example

    -module(seq_tests).

    -include_lib("eunit/include/eunit.hrl").

    %% _test suffix is meaningful
    one_to_five_test() ->
      ?assertEqual([1,2,3,4,5], lists:seq(1, 5)).

## QuickCheck

* Spot patterns in tests
* Generators randomly generate sets of value used as test data
