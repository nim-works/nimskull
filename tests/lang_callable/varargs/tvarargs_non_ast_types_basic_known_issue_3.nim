discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`. Merge these into
    test with similar root name once working.
  '''
  knownIssue: "the compiler can't handle zero arg varargs match"
"""

block non_trailing_varargs:
  # xxx: this should probably be at a lower precendence
  proc foo(v: varargs[int], trailing: string): int =
    v.len

  doAssert foo("foo") == 0, "leading match with zero candidates"