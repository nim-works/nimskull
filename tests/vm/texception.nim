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
  