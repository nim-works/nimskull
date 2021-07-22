discard """
description: "enumeration int values must be assigned in ascending order."
errormsg: "invalid order in enum 'b'"
line: 11
column: 9
"""

type
  Abc = enum
    a = 7,
    b = 2,
    c = 1
