discard """
  description: "Tests for math.signbit on NaN values"
  targets: "c js vm"
  knownIssue.c: '''
    unhandled exception: tmath.nim(255, 5) `not signbit(NaN)`
  '''
"""

import std/math

template main() =
  block signbitNan:
    doAssert not signbit(NaN)

    let x1 = NaN
    let x2 = -NaN
    let x3 = -x1

    doAssert isNaN(x1)
    doAssert isNaN(x2)
    doAssert isNaN(x3)
    doAssert not signbit(x1)
    doAssert signbit(x2)
    doAssert signbit(x3)

  block copySignNan:
    doAssert copySign(-1.0, NaN) == 1.0
    doAssert copySign(10.0, NaN) == 10.0

    doAssert copySign(NaN, NaN).isNaN
    doAssert copySign(-NaN, NaN).isNaN
    doAssert copySign(NaN, -NaN).isNaN
    doAssert copySign(-NaN, -NaN).isNaN
    doAssert copySign(NaN, 0.0).isNaN
    doAssert copySign(NaN, -0.0).isNaN
    doAssert copySign(-NaN, 0.0).isNaN
    doAssert copySign(-NaN, -0.0).isNaN

    doAssert copySign(-1.0, NaN) == 1.0
    doAssert copySign(-1.0, -NaN) == -1.0
    doAssert copySign(1.0, copySign(NaN, -1.0)) == -1.0

static: main()
main()