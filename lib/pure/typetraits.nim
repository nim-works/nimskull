#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Nim Contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module defines compile-time reflection procs for
## working with types.
##
## Unstable API.

import system/magics except error
import std/private/since
export system.`$` # for backward compatibility

type HoleyEnum* = (not Ordinal) and enum ## Enum with holes.
type OrdinalEnum* = Ordinal and enum ## Enum without holes.

runnableExamples:
  type A = enum a0 = 2, a1 = 4, a2
  type B = enum b0 = 2, b1, b2
  assert A is enum
  assert A is HoleyEnum
  assert A isnot OrdinalEnum
  assert B isnot HoleyEnum
  assert B is OrdinalEnum
  assert int isnot HoleyEnum
  type C[T] = enum h0 = 2, h1 = 4
  assert C[float] is HoleyEnum

export magics.name, magics.arity, magics.genericHead, magics.stripGenericParams
export magics.supportsCopyMem, magics.supportsZeroMem
export magics.isNamedTuple

template pointerBase*[T](_: typedesc[ptr T | ref T]): typedesc =
  ## Returns `T` for `ref T | ptr T`.
  runnableExamples:
    assert (ref int).pointerBase is int
    type A = ptr seq[float]
    assert A.pointerBase is seq[float]
    assert (ref A).pointerBase is A # not seq[float]
    assert (var s = "abc"; s[0].addr).typeof.pointerBase is char
  T

export magics.distinctBase

since (1, 1):
  template distinctBase*[T](a: T, recursive: static bool = true): untyped =
    ## Overload of `distinctBase <#distinctBase,typedesc,static[bool]>`_ for values.
    runnableExamples:
      type MyInt = distinct int
      type MyOtherInt = distinct MyInt
      doAssert 12.MyInt.distinctBase == 12
      doAssert 12.MyOtherInt.distinctBase == 12
      doAssert 12.MyOtherInt.distinctBase(false) is MyInt
      doAssert 12.distinctBase == 12
    when T is distinct:
      distinctBase(typeof(a), recursive)(a)
    else: # avoids hint ConvFromXtoItselfNotNeeded
      a

  export magics.tupleLen

  template tupleLen*(t: tuple): int =
    ## Returns the number of elements of the tuple `t`.
    ##
    ## **See also:**
    ## * `tupleLen proc <#tupleLen,typedesc>`_
    runnableExamples:
      doAssert tupleLen((1, 2)) == 2

    tupleLen(typeof(t))

  template get*(T: typedesc[tuple], i: static int): untyped =
    ## Returns the `i`-th element of `T`.
    # Note: `[]` currently gives: `Error: no generic parameters allowed for ...`
    runnableExamples:
      doAssert get((int, int, float, string), 2) is float

    typeof(default(T)[i])

  type StaticParam*[value: static type] = object
    ## Used to wrap a static value in `genericParams <#genericParams.t,typedesc>`_.

since (1, 3, 5):
  template elementType*(a: untyped): typedesc =
    ## Returns the element type of `a`, which can be any iterable (over which you
    ## can iterate).
    runnableExamples:
      iterator myiter(n: int): auto =
        for i in 0 ..< n:
          yield i

      doAssert elementType(@[1,2]) is int
      doAssert elementType("asdf") is char
      doAssert elementType(myiter(3)) is int

    typeof(block: (for ai in a: ai))

export magics.rangeBase, magics.isCyclical

import std/macros

macro enumLen*(T: typedesc[enum]): int =
  ## Returns the number of items in the enum `T`.
  runnableExamples:
    type Foo = enum
      fooItem1
      fooItem2

    doAssert Foo.enumLen == 2

  let bracketExpr = getType(T)
  expectKind(bracketExpr, nnkBracketExpr)
  let enumTy = bracketExpr[1]
  expectKind(enumTy, nnkEnumTy)
  result = newLit(enumTy.len - 1)

since (1, 1):
  macro genericParams*(T: typedesc): untyped =
    ## Returns the tuple of generic parameters for the generic type `T`.
    ##
    ## **Note:** For the builtin array type, the index generic parameter will
    ## **always** become a range type.
    runnableExamples:
      type Foo[T1, T2] = object

      doAssert genericParams(Foo[float, string]) is (float, string)

      type Bar[N: static float, T] = object

      doAssert genericParams(Bar[1.0, string]) is (StaticParam[1.0], string)
      doAssert genericParams(Bar[1.0, string]).get(0).value == 1.0
      doAssert genericParams(seq[Bar[2.0, string]]).get(0) is Bar[2.0, string]
      var s: seq[Bar[3.0, string]]
      doAssert genericParams(typeof(s)) is (Bar[3.0, string],)

      doAssert genericParams(array[10, int]) is (range[0..9], int)
      var a: array[10, int]
      doAssert genericParams(typeof(a)) is (range[0..9], int)

    let desc = getTypeInst(T)
    expectKind(desc, nnkBracketExpr)
    let typ = getType(desc[1]) # skip aliases

    result = newNimNode(nnkTupleConstr)
    case typ.typeKind
    of ntyGenericInst:
      # fetch all instnatiation parameters
      for i in 1..<typ.len:
        let op = getTypeInst(typ[i])
        # ``getTypeInst`` loses the staticness, so `typ` has to be queried
        # instead
        if typ[i].typeKind == ntyStatic:
          result.add nnkBracketExpr.newTree(bindSym"StaticParam", op)
        else:
          result.add op
    of ntyPtr, ntyRef, ntyVar, ntySequence, ntyOpenArray, ntyVarargs, ntySet,
       ntyUncheckedArray:
      result.add typ[1]
    of ntyRange:
      result.add nnkBracketExpr.newTree(bindSym"StaticParam", typ[1])
      result.add nnkBracketExpr.newTree(bindSym"StaticParam", typ[2])
    of ntyArray:
      var len = getTypeInst(typ[1])
      if len.kind == nnkInfix:
        # create a proper range type constructor
        len = nnkBracketExpr.newTree(bindSym"range", len)

      result = nnkTupleConstr.newTree(
        len,
        typ[2])
    else:
      error("not an instantiated generic type", T)

proc hasClosureImpl(n: NimNode): bool = discard "see compiler/vmops.nim"

proc hasClosure*(fn: NimNode): bool {.since: (1, 5, 1).} =
  ## Return true if the func/proc/etc `fn` has `closure`.
  ## `fn` has to be a resolved symbol of kind `nnkSym`. This
  ## implies that the macro that calls this proc should accept `typed`
  ## arguments and not `untyped` arguments.
  expectKind fn, nnkSym
  result = hasClosureImpl(fn)
