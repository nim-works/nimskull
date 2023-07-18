discard """
  targets: "c js vm"
  description: '''
    Regression test for two constant array construction expressions being
    lifted into the *same* constant, despite their float contents having
    different bit representations
  '''
"""

import std/math

type Obj = object
  x: float

proc test() =
  # use a wrapper procedure in order to test with locals, and not globals
  var
    a = [-0.0, 0.0]
    b = [0.0, 0.0]

  # the array values are compile-time known, meaning that they can be lifted
  # into constants, but they must not be erroneously collapsed into a single
  # one
  doAssert classify(a[0]) == fcNegZero
  doAssert classify(a[1]) == fcZero
  doAssert classify(b[0]) == fcZero
  doAssert classify(b[1]) == fcZero

  # for completeness, also test with tuples and objects:
  var
    c = (-0.0, 0.0)
    d = (0.0, 0.0)

  doAssert classify(c[0]) == fcNegZero
  doAssert classify(c[1]) == fcZero
  doAssert classify(d[0]) == fcZero
  doAssert classify(d[1]) == fcZero

  # test with objects:
  var
    e = Obj(x: -0.0)
    f = Obj(x: 0.0)

  doAssert classify(e.x) == fcNegZero
  doAssert classify(f.x) == fcZero

test()