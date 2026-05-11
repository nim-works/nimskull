discard """
  description: '''
    Tests to make sure type relationship correctly takes effects into
    consideration.
  '''
  target: native
"""

# procedures with inferred exception effects
proc test1() = raise newException(CatchableError, "")
proc test2() = raise newException(ValueError, "")
proc test3() = raise newException(IOError, "")

# procedures with declared exception effects
proc test1e() {.raises: [CatchableError].} = discard
proc test2e() {.raises: [ValueError].} = discard
proc test3e() {.raises: [IOError].} = discard

# sub-typing
doAssert typeof(test2) is typeof(test1)
doAssert typeof(test3) is typeof(test1)
# ... but not the other way around
doAssert typeof(test1) isnot typeof(test2)
doAssert typeof(test3) isnot typeof(test2)
# sibling exception types are not compatible
doAssert typeof(test2) isnot typeof(test3)
doAssert typeof(test3) isnot typeof(test2)

# declared vs. inferred exception effects are the same, type wise
doAssert typeof(test1e) is typeof(test1)
doAssert typeof(test1)  is typeof(test1e)
doAssert typeof(test2e) is typeof(test2)
doAssert typeof(test2)  is typeof(test2e)

doAssert typeof(test2) is    proc() {.raises: [CatchableError], nimcall.}
doAssert typeof(test3) is    proc() {.raises: [CatchableError], nimcall.}
doAssert typeof(test2) isnot proc() {.raises: [IOError], nimcall.}
