discard """
  description: '''
    Ensure that exiting an exception handler via unstructured control-flow
    (``break``) works and properly clears the current exception.
  '''
  output: "done"
"""

var steps: seq[int]

block exit:
  try:
    raise ValueError.newException("a")
  except ValueError as e:
    steps.add 1
    if e.msg == "a":
      # an unstructured exit of the except block needs to pop the current
      # exception
      break exit
    else:
      doAssert false, "unreachable"
  doAssert false, "unreachable"

steps.add 2

doAssert getCurrentException() == nil, "current exception was cleared"
doAssert steps == [1, 2]

echo "done" # ensures that the assertions weren't jumped over
