discard """
  description: "Test varargs related overload resolution and precedence"
  knownIssue: "compiler varargs arg parsing is entirely broken"
"""


block no_param_candidate_over_empty_varargs:
  ## This must be an error because there is no way to call the zero-ary `foo`
  ## in an unambiguous fashion.
  proc foo(): string = "no param candidate"
  proc foo(a: varargs[string]): string = "empty varargs"

  doAssert foo() == "no param candidate"

block openarray_param_candidate_matches_over_varargs:
  proc foo(a: openarray[int]): string = "openarray"
  proc foo(a: varargs[int]): string = "varargs"

  doAssert foo(1) == "varargs"
  doAssert foo(1, 2) == "varargs", "should not match openarray"
  doAssert foo([1, 2]) == "openarray"
  doAssert foo(@[1, 2]) == "openarray"

block array_param_candidate_matches_over_varargs:
  proc foo(a: array[1, int]): string = "array"
  proc foo(a: varargs[int]): string = "varargs"

  doAssert foo(1) == "varargs"
  doAssert foo(1, 2) == "varargs", "should not match array"
  doAssert foo([1, 2]) == "array"