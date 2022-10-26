discard """
targets: "c js"
labels: "codegen float"
description: '''
  . From https://github.com/nim-lang/Nim/issues/7079
    -0.0 doesn't result in negative zero in VM
  . -0.0 (and expressions like -2.0 * 0.0) should give the floating point
    value for negative zero, but it gives the value for positive zero.
  . Seems like it's only an issue in the VM
'''
"""

import math
let x = -0.0
doAssert classify(x) == fcNegZero
doAssert classify(1 / -0.0) == fcNegInf
