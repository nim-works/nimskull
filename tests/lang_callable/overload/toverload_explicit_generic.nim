discard """
  description: '''
    The '[]' syntax is used to request overload resolution to only consider
    generic routines
  '''
"""

block single_param:
  proc p(x: int): int =
    result = x + 1

  proc p[T](x: T): T =
    result = x + 2

  # the non-generic overload would match better, but the empty brackets force
  # a generic routine to be chosen
  doAssert p[](1) == 3

block nullary_generic:
  proc p(x: float64): int =
    result = 1

  proc p[](x: float32): int =
    result = 2

  # the call would be ambiguous would a generic overload not be requested
  doAssert p[](1) == 2

block generic_parameter_with_default:
  proc p(x: int): int =
    result = x + 1

  # XXX: use the below instead, once it doesn't cause redefinition errors
  #      anymore
  # proc p[T = int](x: int): int =
  #   doAssert T is int
  #   result = x + 2

  proc p[T = int; U = int](x: T): T =
    doAssert T is int
    doAssert U is int
    result = x + 2

  # the freestanding `U` type parameter has a default value, so this works
  doAssert p[](1) == 3