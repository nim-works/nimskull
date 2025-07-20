discard """
  description: '''
    An exception raised by a suspend block is not caught by exception handlers
    enclosing the 'suspend' within the routine.
  '''
"""

proc test() {.raises: [ValueError].} =
  try:
    suspend void, cont:
      raise ValueError.newException("")
  except ValueError:
    doAssert false

doAssertRaises ValueError:
  test()
