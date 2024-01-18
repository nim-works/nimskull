discard """
  description: '''
    Tests for assignments where an exception is raised during evaluation of
    the right-hand side
  '''
"""

block unobservable_rvo_assignment:
  # assigning the result of an RVO-using call to a variable that is also used
  # as one of the arguments must not be oversable when the call raises an
  # exception
  proc raiseEx(x: array[4, int]): array[4, int] =
    result[0] = 1
    raise CatchableError.newException("")

  proc test() =
    var x: array[4, int]
    try:
      x = raiseEx(x)
    except CatchableError:
      when defined(c):
        # XXX: the C backend is the only one using RVO at the moment
        doAssert x[0] == 1, "the behaviour is correct now; fix the assert"
      else:
        doAssert x[0] == 0, "handler observed changed value"

    when defined(c):
      doAssert x[0] == 1, "the behaviour is correct now; fix the assert"
    else:
      doAssert x[0] == 0, "following statement observed changed value"

  test()

block primitive_type_assignment:
  # assignments to locations of primitive type have no effect when the right-
  # hand is a call expression raising an exception
  proc raiseEx(): int =
    result = 0
    raise CatchableError.newException("")

  proc test() =
    var x = 1
    try:
      x = raiseEx()
    except CatchableError:
      doAssert x == 1

    doAssert x == 1

  test()