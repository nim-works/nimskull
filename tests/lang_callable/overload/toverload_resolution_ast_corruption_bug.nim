discard """
  description: '''
    Regression test for argument expressions in call expressions passed to
    untyped parameters being typed when there exists another overload accepting
    a typed parameter in the same position.
  '''
"""

proc test1(x: int, y: int) =
  discard

proc test2(x: int, y: int) =
  discard

template test2(x, y: untyped) =
  doAssert astToStr(x) == "test1(1 + 3, 2)"

test2(test1(1 + 3, 2), 1)
