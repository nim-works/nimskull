discard """
  action: compile
  description: '''
    Range types depending on type variables must work in routine signatures
  '''
"""

import std/typetraits

proc typeNameLen(x: typedesc): int =
  x.name.len

proc f(t: typedesc, rng: range[-typeNameLen(t) .. typeNameLen(t)]): int =
  result = rng.type.high - rng.type.low

type
  MyType = object # only provides the name
  Typ    = object # shorter name

static:
  # test non-range argument type:
  doAssert f(MyType, 0) == 12

  # check that the formal range is available when computing the type
  # relationship formal and argument type:
  var a: range[4..8] # overlapping range -> matches
  doAssert f(MyType, a) == 12

  var b: range[10..12] # doesn't overlap -> no match
  doAssert not compiles(f(MyType, b) == 12)

  # test with a different name:
  doAssert f(Typ, 0) == 6
  doAssert not compiles(f(Typ, a) == 6)