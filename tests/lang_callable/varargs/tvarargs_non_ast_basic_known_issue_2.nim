discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`. Merge these into
    test with similar root name once working.
  '''
  knownIssue: "the compiler can't handle an array for a second varargs param"
"""

block multiple_varargs:
  block same_types:
    proc foo(v: varargs[int], w: varargs[int]): (int, int) =
      (v.len, w.len)

    let (v, w) = foo(1, 2, 3, [4, 5])
    doAssert v == 3
    doAssert w == 2

    block first_vararg_will_match_all:
      # xxx: we should probably detect this and hint?
      let (v, w) = foo(1, 2, 3, 4, 5)
      doAssert v == 5, "v: " & $v
      doAssert w == 0, "w: " & $w