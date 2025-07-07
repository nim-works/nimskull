discard """
  description: '''
    Ensures inner routines are properly detected as closing over something.
  '''
"""

# forwarded .closure routine used in inner routine
proc test1() =
  proc inner() {.closure.}
  proc inner2() =
    # ^^ inferred as a closure, because `inner` might close over something
    inner()

  var x = 0
  proc inner() =
    echo x

  inner2()

test1()

# forwarded .closure routine that doesn't actually close over something
proc test2() =
  proc inner() {.closure.}
  proc inner2() =
    # ^^ inferred as a closure, because `inner` might close over something
    inner()

  static:
    doAssert inner2 isnot (proc() {.nimcall.})

  proc inner() =
    discard

  inner2()

test2()

# forwarded inner routine with unspecified calling convention
proc test3() =
  proc inner()
  proc inner2() =
    # inferred as .closure because `inner` might close over something
    inner()

  static:
    doAssert inner2 isnot (proc() {.nimcall.})

  proc inner() =
    # ... but doesn't in reality
    discard

  inner2()

test3()

# forwarded inner routine with unspecified calling convention (2)
proc test4() =
  proc inner()
  proc inner2() =
    # inferred as .closure because `inner` might close over something
    inner()

  static:
    doAssert inner2 isnot (proc() {.nimcall.})

  var x = 0
  proc inner() =
    # ... and does
    echo x

  inner2()

test4()

# forwarded inner routine with explicit, non-.closure calling convention
proc test5() =
  proc inner() {.nimcall.}
  proc inner2() {.nimcall.} =
    inner()

  proc inner() =
    discard

  inner2()

test5()

# generic inner routine defined and used in other inner routine
proc test6() =
  var x = 0

  proc inner() =
    # `inner` doesn't close over anything *directly*
    proc innerInner[T]() =
      echo x # generic routine closes over something
    innerInner[int]()
    # `inner` still closes over something indirectly, by using `innerInner`

  inner()

test6()
