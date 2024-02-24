discard """
  description: '''
    Tests for non-ast varargs, those that are not `un/typed`, with conversions.
  '''
"""

block conversions_for_non_ast_varargs:
  var called = 0
  proc makeStr(s: string): string = inc called; $s
  proc makeStr(i: int): string = inc called; $i
  proc makeStr(i: int8): string = inc called; $i

  # using `template` below to try different routine kinds, and testing for both
  # trailing and non-trailing varargs.
  template before(v: varargs[string, makeStr], values: openarray[string]) =
    for i, a in v.pairs:
      doAssert a == values[i], a & " is not equal to " & values[i]

  template after(values: openarray[string], v: varargs[string, makeStr]) =
    for i, a in v.pairs:
      doAssert a == values[i], a & " is not equal to " & values[i]

  template calledCheck(count: int, body: untyped) {.dirty.} =
    called = 0
    body
    doAssert called == count, "called: " & $called

  # don't be tempted to dry out the code below, because we need to test vararg
  # passing direct parser input without intermediate dispatch mucking that up

  block single_string:
    let expected = ["test"]
    calledCheck 1:
      before("test", expected)
    calledCheck 1:
      after(expected, "test")

  block multiple_string:
    let expected = ["test", "best"]
    calledCheck 2:
      before("test", "best", expected)
    calledCheck 2:
      after(expected, "test", "best")

  block multiple_differing:
    let expected = ["test", "1", "-3"]
    calledCheck 3:
      before("test", 1, -3'i8, expected)
    calledCheck 3:
      after(expected, "test", 1, -3'i8)

  block as_array:
    # expectation is that the argument is semchecked before being tested for
    # varargs match, so it's already an `openArray[string]` and should
    # therefore match `varargs[string]`.
    let expected = ["1", "test"]
    calledCheck 0:
      before(["1", "test"], expected)
    calledCheck 0:
      after(expected, ["1", "test"])