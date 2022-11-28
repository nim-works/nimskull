discard """
  targets: "c"
"""

import typeinfo

var x = ""
discard (getPointer(toAny(x)))
