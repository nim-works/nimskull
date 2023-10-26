discard """
description: '''
Basics of boolean types
'''
"""

# overlaps heavily with literals, not going to cover here

let
  t = true
  f = false
doAssert t and t, "and operation"
doAssert t or f,  "or operation"
doAssert f xor t, "xor operation"