discard """
description: '''
An enumeration defines a type with a closed set of values. These values are
have an identifier, are ordered, and have corresponding integer and string
values.
'''
target: "c cpp js"
"""

block:
  # enums int values are zero based indexed by default, `ord` is used
  # to retrieve their int value
  type
    Abc = enum
     a, b, c
  doAssert ord(Abc.a) == 0, "1st enum int value corresponds to 0"
  doAssert ord(Abc.b) == 1, "2nd enum int value corresponds to 1"
  doAssert ord(Abc.c) == 2, "3rd enum int value corresponds to 2"

block:
  # access only needs to be qualified if ambiguous -- here it isn't
  # xxx: cover pure elsewhere, it's a legacy thing that should be removed
  type
    Abc = enum
     a, b, c
  doAssert ord(b) == 1, "unqualified access"

block:
  # enum string values are derived from their name by default, use `$` for
  # conversion to a string
  type
    Abc = enum
     a, b, c
  doAssert $a == "a", "value's string derives from the name by default - a"
  doAssert $b == "b", "value's string derives from the name by default - b"
  doAssert $c == "c", "value's string derives from the name by default - c"

block:
  # enum int values can be assigned
  type
    Abc = enum
     a = 2,
     b = 3,
     c = 5
  doAssert ord(a) == 2, "enum assigned int value - a"
  doAssert ord(b) == 3, "enum assigned int value - b"
  doAssert ord(c) == 5, "enum assigned int value - c"

block:
  # enum string values can be assigned
  type
    Abc = enum
     a = "apple",
     b = "zebra",
     c = "panda"
  doAssert $a == "apple", "enum assigned string value - a"
  doAssert $b == "zebra", "enum assigned string value - b"
  doAssert $c == "panda", "enum assigned string value - c"

block:
  # enum int and string values can be assigned and gaps are filled, int is
  # incremented and strings revert to deriving from the name
  type
    Abc = enum
     a = (0, "ape"),
     b = ("see"),
     c = 5
  doAssert ord(a) == 0 and $a == "ape", "assigned int or string values - a"
  doAssert ord(b) == 1 and $b == "see", "assigned int or string values - b"
  doAssert ord(c) == 5 and $c == "c",   "assigned int or string values - c"

block:
  # enum values can be assigned the same string values, and they can be empty
  # xxx: this seems like a bug/design flaw
  type
    Abc = enum
     a = "",
     b = "",
     c = ""
  doAssert $a == "", "enum assigned an empty string value - a"
  doAssert $b == "", "enum assigned an empty string value - b"
  doAssert $c == "", "enum assigned an empty string value - c"
