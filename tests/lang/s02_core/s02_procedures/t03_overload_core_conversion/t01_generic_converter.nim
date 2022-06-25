discard """
description: '''
Converters can be used with generic types without specifying explicit
parameters
'''
joinable: false
"""

when not defined(cpp) or defined(tryBrokenSpecification):
  # C++ backend generates invalid code for the converter
  type
    Generic[T] = object
      s: seq[T]

  converter toGeneric[T](s: seq[T]): Generic[T] = Generic[T](s: s)

  var x = Generic @[1, 2, 3]

  doAssert x.s == @[1, 2, 3], "Argument copied in converter"
  doAssert x is Generic[int], "Type is inferred on construction"

  doAssert Generic(@[0'u8, 1, 2]) is Generic[uint8]