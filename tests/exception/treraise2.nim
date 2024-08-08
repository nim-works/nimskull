discard """
  description: '''
    Ensure that raising a caught exception from within an exception handler
    works
  '''
"""

proc manualReraise() =
  # raising an already caught exception works and doesn't interfere with how
  # the current exception is set
  try:
    raise CatchableError.newException("1")
  except CatchableError as e:
    raise e

try:
  manualReraise()
except CatchableError as e:
  doAssert e.msg == "1"

doAssert getCurrentException() == nil

# ---------------------------------------------------------------------------
# more complex situation: re-raise an exception not caught by the most nested
# handler

# situation 1: catch within the original handler
try:
  raise CatchableError.newException("1")
except CatchableError as e:
  try:
    try:
      raise CatchableError.newException("2")
    except CatchableError as e2:
      raise e # raise the outer exception again
  except CatchableError as e2:
    doAssert e2.msg == "1"

  # the current exception must still be the same as `e`
  doAssert getCurrentException().msg == "1"

doAssert getCurrentException() == nil

# situation 2: catch outside the original handler
try:
  try:
    raise CatchableError.newException("1")
  except CatchableError as e:
    try:
      raise CatchableError.newException("2")
    except CatchableError as e2:
      raise e # raise the outer exception again
  # the exception propagates outside the original handler
except CatchableError as e:
  doAssert e.msg == "1"

doAssert getCurrentException() == nil
