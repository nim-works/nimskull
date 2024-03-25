discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`. Merge these into
    test with similar root name once working.
  '''
  knownIssue: "the compiler can't handle trailing defaulted/varargs params"
"""

block limit_greediness_of_varargs_parameter_consumption:
  block defaulted_dont_count:
    # xxx: hint that only a named param/array call syntax will work?
    func bar(_: varargs[string], wutboutme: string = "oh hai") =
      doAssert wutboutme == "oh hai", wutboutme

    bar("test", "best", "this", "one too")

  block varargs_dont_count:
    # xxx: hint that only a named param/array call syntax will work?
    func baz(_: varargs[string], wutboutme: varargs[string] = []) =
      doAssert wutboutme == []

    baz("test", "best", "this", "one too")