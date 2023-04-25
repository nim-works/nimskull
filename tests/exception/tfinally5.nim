discard """
  matrix: "--exceptions:goto --panics:on"
  targets: c
"""

block potentially_raising_call_in_finally:
  # a test to make sure that error mode is disabled for the duration of
  # a 'finally' clause. This also has to happen when the finally's body doesn't
  # raise any uncaught exceptions or defects
  proc p2(x: bool) {.noinline.} =
    if x:
      raise CatchableError.newException("")

  proc p() {.raises: [].} = # must have no unhandled exception effects
    var i = 0
    try:
      p2(false) # known to not raise an exception at run-time
      i = 1
    except CatchableError:
      doAssert false

    doAssert i == 1

  var i = 0
  try:
    try:
      # enter the finally with an active exception
      raise CatchableError.newException("")
    finally:
      p() # never raises an exception or defect
      i = 1
  except CatchableError:
    doAssert i == 1
    i = 2

  doAssert i == 2