discard """
description: '''
Two enumerations with values that have equal int values are not equal and
result in a type error.
'''
errormsg: "type mismatch: got <TE1, TE2>"
line: 15
column: 11
"""

type
  TE1 = enum eA, eB
  TE2 = enum eC, eD

assert eA != eC
