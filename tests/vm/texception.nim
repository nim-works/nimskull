discard """
  description: "VM exception handling related tests"
  action: compile
"""

proc someFunc(): int =
  result = 1 # for this test, it's important that the result is initialized
  try:
    raise newException(ValueError, "message")
  except ValueError as err:
    doAssert err.name == "ValueError"
    doAssert err.msg == "message"
    raise

static:
  try:
    discard someFunc()
  except:
    discard

static:
  proc raiseEx() =
    raise ValueError.newException("A")

  proc doSomething() = discard

  try:
    try:
      raiseEx() # it's important that the exception is raised from a
                # function here
    finally:
      doSomething()
      raise ValueError.newException("B") # raise a different exception while
                                         # another exception is already active

  except ValueError as e:
    # the `except` needs to be on the same stack-frame as the `raise` in the
    # `finally` block
    doAssert e.msg == "B"