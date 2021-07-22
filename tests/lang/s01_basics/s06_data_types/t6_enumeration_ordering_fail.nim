discard """
description: '''
An enumeration defines a type with a closed set of values. These values are
have an identifier, are ordered, and have corresponding integer and string
values.
'''
errormsg: "invalid order in enum 'b'"
line: 16
column: 9
"""

# enum int values can be assigned, but must be in ascending order
type
  Abc = enum
    a = 7,
    b = 2,
    c = 1
