discard """
  description: '''
    Ensure that overloads separated by `typed` and `untyped` parameters don't
    result in a tie, if the argument in question is erroneous (i.e., an
    `nkError`).
  '''
  action: compile
"""

import std/macros

macro m(x: typed) =
  discard

macro m(x: untyped) =
  discard

m(missingIdent)