discard """
  description: "Tests for the math module"
  targets: "c js vm"
"""

import std/math

block isNaN:
  doAssert isNaN(NaN)
  doAssert not isNaN(0.0)
  doAssert not isNaN(3.1415926)