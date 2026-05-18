discard """
  targets: "c"
"""

import std/typeinfo

var x = ""
discard getString(toAny(x))
