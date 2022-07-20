discard """
  targets: "c cpp js"
"""

var x = 10
discard atomicInc(x)
doAssert x == 11
discard atomicDec(x)
doAssert x == 10
