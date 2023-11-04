discard """
  description: '''
    Regression test for resolved concept types used as the return type
    being overriden with a non-concept type on `result` assignment
  '''
  action: compile
"""

type
  Generic[T] = concept x
    x is T

proc test(x: Generic): typeof(x) =
  result = x

static:
  # the below failed previously, as the result assignment changed the return
  # type to ``int`` instead of leaving it as the ``Generic[int]``
  doAssert typeof(test(Generic[int](1))).T is int