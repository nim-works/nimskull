discard """
description: '''
Enumeration int values must be assigned in ascending order.
'''
errormsg: "invalid order in enum 'b'"
line: 13
column: 9
"""

type
  Abc = enum
    a = 7,
    b = 2,  # This is not valid since the second enum int value is less than the first
    c = 1   # This would also not be valid
