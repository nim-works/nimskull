discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`. Merge these into
    test with similar root name once working.
  '''
  knownIssue: "the compiler doesn't yet support limiting varargs greed"
"""

import std/macros

block limit_greediness_of_varargs_parameter_consumption:
  ## following non-defaulted/vararg params means: varargs must stop matching in
  ## order to leave enough for following params
  block match_args_with_enough_for_each_trailing_param:
    func foo(_: varargs[string], wutboutme: string) =
      doAssert wutboutme == "we didn't forget"

    foo("test", "best", "this", "one too", "we didn't forget")