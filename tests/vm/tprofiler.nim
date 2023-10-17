discard """
  description: '''
    Ensure that the VM profiler doesn't crash with exceptional control-flow
  '''
  matrix: "--profileVM"
"""

proc doSomething() =
  let f = 0.5
  # further call nesting:
  discard $f

proc p3() =
  # call some procedure
  doSomething()

  raise CatchableError.newException("")

proc p2() =
  try:
    p3()
  finally:
    # procedure call in finally block during unwinding
    doSomething()

proc p1() =
  try:
    p2()
  except:
    # procedure call in exception handler
    doSomething()

static:
  p1()