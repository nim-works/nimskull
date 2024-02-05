discard """
  description: '''
    A test for tracking the (uncertain) behaviour of entering a `finally`
    clause through an exception, handling the exception within the
    `finally`, and then leaving through structured control-flow.
  '''
  matrix: "-d:noSignalHandler"
  knownIssue: "true"
"""

# XXX: disabling the signal handler is currently necessary for the test
#      to crash rather than entering an infinite loop

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
    # unwinding cannot continue when leaving the finally, since no exception
    # is active anymore
  steps.add 3
except CatchableError:
  # never reached, since the exception was handled above
  doAssert false, "unreachable"

doAssert steps == [1, 2, 3]