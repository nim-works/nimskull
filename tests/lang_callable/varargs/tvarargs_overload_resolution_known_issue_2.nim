discard """
  description: "Test varargs related overload resolution and precedence"
  knownIssue: "compiler varargs arg parsing is entirely broken"
"""


block arity_has_greater_precedence_than_genericicity:
  block no_param_nullary_generic_candidate_over_empty_varargs:
    proc foo[](): string = "no param nullary generic candidate"
    proc foo(a: varargs[string]): string = "empty varargs"

    doAssert foo() == "no param nullary generic candidate"