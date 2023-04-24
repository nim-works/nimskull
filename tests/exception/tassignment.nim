discard """
  targets: "c js vm"
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
      doAssert x[0] == 0, "handler observed changed value"

    doAssert x[0] == 0, "following statement observed changed value"

  test()