discard """
  description: "Tests for untyped varargs parameters"
  knownIssue: "Compiler can't handle limiting varargs matching"
"""


block match_args_with_enough_for_each_trailing_param:
  template foo(varargs[typed], wutboutme: string) =
    doAssert wutboutme == "we didn't forget"

  foo("test", "best", "this", "one too", "we didn't forget")