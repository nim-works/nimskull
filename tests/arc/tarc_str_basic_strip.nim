discard """
  matrix: "--gc:arc"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/17173
      strbasics.strip SIGBUS with --gc:arc
  '''
"""

import std/strbasics


var a = "  vhellov   "
strip(a)
doAssert a == "vhellov"