discard """
  description: '''
    Ensure that a finally clause can fully handle an exception by re-raising
    and catching it
  '''
  knownIssue.c js vm: '''
    Not implemented properly by the code generator and/or runtime
  '''
"""

# XXX: it's questionable whether this really should work / be allowed

var steps: seq[int]

try:
  try:
    raise CatchableError.newException("a")
  finally:
    try:
      raise # re-raise the active exception
    except CatchableError:
      # catch the exception
      steps.add 1
      # leaving this except handler means that the exception is handled
    steps.add 2
    doAssert getCurrentException() == nil
    # unwinding doesn't continue when leaving the finally
  steps.add 3
except CatchableError:
  # never reached, since the exception was handled above
  doAssert false, "unreachable"

doAssert steps == [1, 2]