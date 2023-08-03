discard """
  description: "Tests for the cyclic type detection"
  action: compile
"""

import std/typetraits

template test(t: untyped) =
  {.line.}:
    static: doAssert isCyclical(t)

template testNot(t: untyped) =
  {.line.}:
    static: doAssert not isCyclical(t)

# primitive types are never cyclic
testNot int
testNot float
testNot void

type
  NonCyclicObj = object
    x: seq[NonCyclicObj]
  NonCyclicRef = ref object

# no reference cycle is possible
testNot NonCyclicObj
testNot NonCyclicRef

type Cyclic = ref object
  x: Cyclic

test    Cyclic
# the tuple/seq/array is not part of the reference cycle, their elements are:
testNot (Cyclic,)
testNot seq[Cyclic]
testNot array[1, Cyclic]

type
  AliasedInt = int
  AliasedCyclic = Cyclic

# type aliases use the aliased type
testNot AliasedInt
test    AliasedCyclic

type Cyclic2 = object
  x: ref (Cyclic2, int)

# a container tuple/seq/etc. storing a ``ref`` type matching the container can
# be part of a reference cycle
test    (Cyclic2, int)
test    (ref (Cyclic2, int))

type
  Acyclic[T] {.acyclic.} = object
    # no cycles are possible through a location of type `Acyclic`
    a: ref T

  A = object
    x: ref Acyclic[A] # indirect cycle through a different type
    y: ref A

  B = object
    x: ref Acyclic[B]

test    A
testNot B

type
  WithCursor = object
    x {.cursor.}: ref WithCursor

testNot WithCursor

# closures don't have a statically known environment type, so the analysis has
# to be pessimistic and assume that the dynamic environment contains
# `WithClosure` somewhere
type
  WithClosure = object
    x: proc () {.closure.}

test    WithClosure

# same with types that can be inherited from
type
  Inheritable = object of RootObj
  Final {.final.} = object of Inheritable
  AcyclicBase {.acyclic.} = object of RootObj

  Test[T] = object
    x: ref T

# inspecting a non-ref type that can be inherited from is also treated as cyclic

test    Inheritable
test    Test[Inheritable]

testNot Final
testNot Test[Final]

# using acyclic for an inheritable type overrides both the super- and sub-types
testNot AcyclicBase
testNot Test[AcyclicBase]

# `distinct` types are treated as their base type during the cyclic analysis
type
  DRef = distinct WithDRef
  WithDRef = ref object
    x: DRef

  DRef2 = distinct WithDRef2
  WithDRef2 = object
    x: ref DRef2

test    DRef
test    WithDRef
test    DRef2
test    WithDRef2

# ``UncheckedArray``s are not considered because their contents are not traced
# by the cycle collector
type
  WithUncheckedArray = object
    x: UncheckedArray[ref WithUncheckedArray]

testNot WithUncheckedArray
testNot (ref WithUncheckedArray)