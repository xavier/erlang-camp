-module(editor_tests).

-include_lib("eunit/include/eunit.hrl").

edit_delete_test() ->
  ?assertEqual("bc", editor:edit("abc", [delete])).

edit_skip_delete_test() ->
  ?assertEqual("ab", editor:edit("abc", [skip, skip, delete])).

edit_replace_test() ->
  ?assertEqual("xbc", editor:edit("abc", [{replace, $x}])).

edit_insert_test() ->
  ?assertEqual("xabc", editor:edit("abc", [{insert, $x}])).

edit_add_test() ->
  ?assertEqual("axbc", editor:edit("abc", [{add, $x}])).

edit_acceptance_test() ->
  ?assertEqual("one test", editor:edit("no test", [{insert, $o}, skip, skip, {replace, $e}])).


edits_trivial_test() ->
  ?assertEqual([{replace, $x}, {replace, $y}, {replace, $z}], editor:edits("abc", "xyz")).

edits_from_shorter_than_to_test() ->
  ?assertEqual([{replace, $x}, {replace, $y}, {insert, $z}], editor:edits("ab", "xyz")).

edits_from_longer_than_to_test() ->
  ?assertEqual([{replace, $x}, {replace, $y}, delete], editor:edits("abc", "xy")).

edits_same_string_test() ->
  ?assertEqual([skip, skip, skip], editor:edits("abc", "abc")).

edits_string_skip_test() ->
  ?assertEqual([skip, {replace, $x}, skip], editor:edits("abc", "axc")).