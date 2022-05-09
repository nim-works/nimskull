discard """
"""

import math
let x = -0.0
doAssert classify(x) == fcNegZero
doAssert classify(1 / -0.0) == fcNegInf