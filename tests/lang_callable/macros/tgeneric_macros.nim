discard """
  description: "Tests for macros with explicit generic parameters"
  action: compile
"""

import std/macros

block generic_parameter_access:
  macro m[T]() =
    # the `T` is accesible as a ``NimNode`` in the macro
    doAssert T is NimNode
    doAssert T.typeKind == ntyInt

  m[int]()