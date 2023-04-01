discard """
  description: "Tests for macros with default parameters"
  action: compile
"""

import std/macros

block mixed_explicit_call:
  # the macro is invoked via call/command syntax and has parameters with and
  # without defaults
  macro m(a: int; b: untyped = 2) =
    doAssert b.intVal == 2

  m(1)

block no_call_syntax:
  # invoking the macro without call syntax works too
  macro m(a: untyped = 1; b: untyped = 2) =
    doAssert a.intVal == 1
    doAssert b.intVal == 2

  m

block generic_no_call_syntax:
  # invoking the macro without call syntax also works for generic
  # macros
  macro m[T](a: untyped = 1) =
    # XXX: the ``T`` is not (yet) accesible as a ``NimNode`` in the body
    #doAssert T.typeKind == ntyInt
    doAssert a.intVal == 1

  m[int]

# XXX: this doesn't work, but it could. If analysis of the default value would
#      be deferred until instantiation time (because that's when it's known
#      whether or not the parameter type is ``untyped``), if the parameter type
#      depends on a generic parameter.
#      For related discussion about whether this should be an error instead,
#      see: https://github.com/nim-works/nimskull/pull/615#discussion_r1155224154
when false: #block dependent_defaults:
  macro m[T](a: T = 1): untyped =
    if a.getType.isNil:
      result = ident"false" # the input is untyped
    else:
      result = ident"true"

  static:
    doAssert m[untyped]() == false
    doAssert m[int]() == true

block mixed_explicit_call_in_generic:
  # immediate macros with default parameters are also evaluated during early
  # analysis of generic routines
  var i {.compileTime.} = 0

  macro m(a: untyped; b: untyped = 2) =
    doAssert a.intVal == 1
    doAssert b.intVal == 2
    inc i

  proc p[T]() =
    m(1)

  # the macro is an immediate macro, so its evaluated during early generic
  # routine body analysis -- no instance of ``p`` has to be created

  static:
    doAssert i == 1 # make sure the macro was really evaluated