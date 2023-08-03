discard """
  targets: "c"
"""

import typeinfo

var x = ""
discard getString(toAny(x))
