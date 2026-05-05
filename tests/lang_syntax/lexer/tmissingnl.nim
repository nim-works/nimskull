discard """
  errormsg: "invalid indentation"
  file: "tmissingnl.nim"
  line: 7
"""

import std/strutils let s: seq[int] = @[0, 1, 2, 3, 4, 5, 6]

#s[1..3] = @[]
