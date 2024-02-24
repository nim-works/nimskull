discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`. Merge these into
    test with similar root name once working.
  '''
  knownIssue: "the compiler can't handle zero arg and multiple varargs matches"
"""

block multiple_varargs:
  block differing_types:
    proc foo(v: varargs[int], w: varargs[bool]): (int, int) =
      (v.len, w.len)

    doAssert foo(1, 2, 3, false) == (3, 1), "3 ints and 1 bool"

    block wraps_in_bracket:
      macro checkStructure(callStmt: typed, v1: openArray[int],
                          v2: openArray[bool]): bool =
        ## check the array/arg(?) conversions performed on varargs
        let call = callStmt[0]
        doAssert call[1].kind == nnkBracket, "bracket for trailing"
        newTree(nnkCall, ident"and",
                        newTree(nnkCall, ident"==", call[1], v1),
                        newTree(nnkCall, ident"==", call[2], v2))

      doAssert checkStructure(foo(1, 2, true, false), [1, 2], [true, false])
      doAssert checkStructure(foo(1, 2), [1, 2], [])
      doAssert checkStructure(foo(true, false), [], [true, false])