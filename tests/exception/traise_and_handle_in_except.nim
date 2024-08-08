discard """
  description: '''
    Ensure that raising and fully handling an exception within an ``except``
    branch works.
  '''
  output: "done"
"""

var steps: seq[int]

try:
  raise ValueError.newException("a")
except ValueError as e:
  steps.add 1
  try:
    raise ValueError.newException("b")
  except ValueError as e:
    steps.add 2
    doAssert e.msg == "b"
    doAssert getCurrentException() == e

  steps.add 3
  # make sure the current exception is still the correct one
  doAssert e.msg == "a"
  doAssert getCurrentException() == e

steps.add 4
doAssert getCurrentException() == nil, "current exception wasn't cleared"
doAssert steps == [1, 2, 3, 4]

echo "done" # make sure the assertions weren't skipped over
