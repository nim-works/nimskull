discard """
  action: "compile"
"""

import std/terminal

proc test() {.raises:[IOError, ValueError].} =
  setBackgroundColor(stdout, bgRed)

test()
