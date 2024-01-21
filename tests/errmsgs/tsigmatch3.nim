discard """
  description: '''
    Ensure that error nodes match against `typed` parameters without error, but
    at a lower priority so as to allow matching against `untyped` overloads.
  '''
"""

import std/macros

macro m(x: typed): untyped =
  newLit "typed"

macro m(x: untyped): untyped =
  newLit "untyped"

doAssert "untyped" == m(missingIdent)