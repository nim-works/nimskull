discard """
  description: "Tests for non-ast varargs, those that are not `un/typed`."
"""

import std/macros

block single_varargs:
  proc foo(v: varargs[int]): int = v.len

  doAssert foo() == 0
  doAssert foo(9) == 1
  doAssert foo(1, 2, 3) == 3

  block a_var_arg_position_can_be_satisfied_by_an_openarray:
    doAssert foo([1, 2, 3, 4]) == 4, "varargs as array"
    doAssert foo(@[1, 2, 3]) == 3, "varargs as sequence"

  macro checkStructure(call: typed, argArray: openArray[int]): bool =
    ## check the array conversion performed on trailing varargs
    newTree(nnkCall, ident"==", call[1], argArray)

  doAssert checkStructure(foo(), []), "empty array for no varargs"
  doAssert checkStructure(foo(9), [9]), "single item array for one varargs"
  doAssert checkStructure(foo(1, 2), [1, 2]), "all items in an array for varargs"


block trailing_varargs:
  block with_leading_param:
    proc foo(leading: string, v: varargs[int]): int =
      v.len

    doAssert foo("foo") == 0, "no varargs provided"
    doAssert foo("foo", 1) == 1, "one varargs provided"
    doAssert foo("foo", 1, 2) == 2, "two varargs provided"
    doAssert foo("foo", [1, 2, 3]) == 3, "3 provided as an array"
    doAssert foo("foo", @[1, 2]) == 2, "2 provided as a sequnce"

  block with_a_leading_defaulted_param:
    proc foo(leading = 3, v: varargs[int]): int =
      doAssert v.len == 0
      leading

    doAssert foo() == 3


block non_trailing_varargs:
  # xxx: this should probably be at a lower precendence
  proc foo(v: varargs[int], trailing: string): int =
    v.len

  block knownIssue:
    # doAssert foo("foo") == 0, "leading match with zero candidates"
    discard "knownIssue: test in `tvarargs_non_ast_types_basic_known_issue_3"

  doAssert foo(1, "foo") == 1, "one leading varargs provided"
  doAssert foo(1, 2, "foo") == 2, "two leading varargs provided"
  doAssert foo([1, 2, 3], "foo") == 3, "3 leading provided as array"
  doAssert foo(@[1, 2], "foo") == 2, "2 leading provided as sequence"


block limit_greediness_of_varargs_parameter_consumption:
  ## following non-defaulted/vararg params means: varargs must stop matching in
  ## order to leave enough for following params
  block match_args_with_enough_for_each_trailing_param:
    discard "knownIssue: test in `tvarargs_non_ast_types_basic_known_issue_1"

  block defaulted_dont_count:
    # xxx: hint that only a named param/array call syntax will work?
    func bar(_: varargs[string], wutboutme: string = "oh hai") =
      doAssert wutboutme == "oh hai", wutboutme

    bar("test", "best", "this", "one too")

    block nor_do_varargs:
      # xxx: hint that only a named param/array call syntax will work?
      func baz(_: varargs[string], wutboutme: varargs[string] = []) =
        doAssert wutboutme == []

      baz("test", "best", "this", "one too")


block multiple_varargs:
  block same_types:
    proc foo(v: varargs[int], w: varargs[int]): (int, int) =
      (v.len, w.len)

    discard "knownIssue: test in `tvarargs_non_ast_types_basic_known_issue_2"

    block first_vararg_will_match_all:
      discard "knownIssue: test in `tvarargs_non_ast_types_basic_known_issue_2"

  block differing_types:
    proc foo(v: varargs[int], w: varargs[bool]): (int, int) =
      (v.len, w.len)

    discard "knownIssue: tests in `tvarargs_non_ast_types_basic_known_issues_4"

    block wraps_in_bracket:
      discard "knownIssue: tests in `tvarargs_non_ast_types_basic_known_issues_4"