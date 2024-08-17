discard """
  action: reject
  nimout: '''
timplicit_conversion_var_parameter.nim(18, 2) Error: type mismatch: got <uint16>
but expected one of:
proc p(x: var uint32)
  first type mismatch at position: 1
  required type for x: var uint32
  but expression 'arg' is immutable, not 'var'

expression: p(arg)
'''
"""

proc p(x: var uint32) = discard

var arg: uint16 # requires widening conversion
p(arg)
