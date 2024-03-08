discard """
  description: '''
    Ensure that leaving an `except` section by raising an exception properly
    updates the current exception.
  '''
  knownIssue.js: "The current exception is not reset properly"
"""

var steps: seq[string]

type Ex = object of CatchableError

proc `=destroy`(x: var Ex) =
  steps.add x.msg

template postcondition() =
  # ensure that the exceptions were destroyed in the correct order
  when defined(gcOrc) or defined(gcArc):
    doAssert steps == ["1", "2"]

# -------------
# test case 1: local raise and local exception handler

proc test1() =
  try:
    try:
      raise Ex.newException("1")
    except Ex as e:
      doAssert getCurrentException() == e
      doAssert e.msg == "1"
      raise Ex.newException("2")
  except Ex as e:
    doAssert getCurrentException() == e
    doAssert e.msg == "2"

  doAssert getCurrentException() == nil

test1()
postcondition()

# -------------
# test case 2: indirect raise and local exception handler

proc raiseEx() =
  raise Ex.newException("2")

proc test2() =
  try:
    try:
      raise Ex.newException("1")
    except Ex as e:
      doAssert getCurrentException() == e
      doAssert e.msg == "1"
      raiseEx()
  except Ex as e:
    doAssert getCurrentException() == e
    doAssert e.msg == "2"

  doAssert getCurrentException() == nil

steps = @[]
test2()
postcondition()

# -------------
# test case 3: indirect raise and non-local exception handler

proc test3() =
  proc inner() =
    try:
      raise Ex.newException("1")
    except Ex as e:
      doAssert getCurrentException() == e
      doAssert e.msg == "1"
      raiseEx()

  try:
    inner()
  except Ex as e:
    doAssert getCurrentException() == e
    doAssert e.msg == "2"

  doAssert getCurrentException() == nil

steps = @[]
test3()
postcondition()
