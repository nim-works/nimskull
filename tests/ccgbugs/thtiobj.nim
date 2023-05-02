discard """
  targets: "c"
  matrix: "--gc:refc; --gc:orc"
"""

import typeinfo

var x = ""
discard getString(toAny(x))
