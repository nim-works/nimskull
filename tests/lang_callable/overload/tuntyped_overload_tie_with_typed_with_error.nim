discard """
  description: '''
    Ensure that overloads separated by `typed` and `untyped` parameters tie,
    resulting in an ambiguity error, if that argument is an `nkError`.

    This isn't ideal, but it works until `untyped` parameters result in eager
    matching over any other parameter type.
  '''
  errormsg: "ambiguous call"
  line: 22
  column: 2
"""

import std/macros

macro m(x: typed) =
  discard

macro m(x: untyped) =
  discard

m(missingIdent)