discard """
errormsg: "6 can't be converted to range 1..5(int8)"
line: 14
description: '''
  . From https://github.com/nim-lang/Nim/issues/2669
    Set literal drop out of range values
  . This silently drops the out of range value, with no error.
    Results in {1i8, 2i8}
  . https://github.com/nim-lang/Nim/commit/0a14b3d1980e116bd40a98d5d2b414a0e1d05a1c
    Check the RHS when building a set
'''
"""

var c: set[range[1i8..5i8]] = {1i8, 2i8, 6i8}

