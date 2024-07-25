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

proc name*(t: typedesc): string {.magic: "TypeTrait".} =
  ## Returns the name of the given type.
  ##
  ## Alias for `system.\`$\`(t) <dollars.html#$,typedesc>`_ since Nim v0.20.
  runnableExamples:
    doAssert name(int) == "int"
    doAssert name(seq[string]) == "seq[string]"

proc arity*(t: typedesc): int {.magic: "TypeTrait".} =
  ## Returns the arity of the given type. This is the number of "type"
  ## components or the number of generic parameters a given type `t` has.
  runnableExamples:
    doAssert arity(int) == 0
    doAssert arity(seq[string]) == 1
    doAssert arity(array[3, int]) == 2
    doAssert arity((int, int, float, string)) == 4

proc genericHead*(t: typedesc): typedesc {.magic: "TypeTrait".} =
  ## Accepts an instantiated generic type and returns its
  ## uninstantiated form.
  ## A compile-time error will be produced if the supplied type
  ## is not generic.
  ##
  ## **See also:**
  ## * `stripGenericParams proc <#stripGenericParams,typedesc>`_
  runnableExamples:
    type
      Foo[T] = object
      FooInst = Foo[int]
      Foo2 = genericHead(FooInst)

    doAssert Foo2 is Foo and Foo is Foo2
    doAssert genericHead(Foo[seq[string]]) is Foo
    doAssert not compiles(genericHead(int))

    type Generic = concept f
      type _ = genericHead(typeof(f))

    proc bar(a: Generic): typeof(a) = a

    doAssert bar(Foo[string].default) == Foo[string]()
    doAssert not compiles bar(string.default)

    when false: # these don't work yet
      doAssert genericHead(Foo[int])[float] is Foo[float]
      doAssert seq[int].genericHead is seq

proc stripGenericParams*(t: typedesc): typedesc {.magic: "TypeTrait".} =
  ## This trait is similar to `genericHead <#genericHead,typedesc>`_, but
  ## instead of producing an error for non-generic types, it will just return
  ## them unmodified.
  runnableExamples:
    type Foo[T] = object

    doAssert stripGenericParams(Foo[string]) is Foo
    doAssert stripGenericParams(int) is int

proc supportsCopyMem*(t: typedesc): bool {.magic: "TypeTrait".}
  ## This trait returns true if the type `t` is safe to use for
  ## `copyMem`:idx:.
  ##
  ## Other languages name a type like these `blob`:idx:.

proc supportsZeroMem*(t: typedesc): bool {.magic: "TypeTrait".}
  ## This trait returns true if using `zeroMem`:idx: on a location of type `t`
  ## brings the location into its "default-initialized" state. This doesn't
  ## imply that using `zeroMem`:idx: on a location already storing a value is
  ## valid.

proc isNamedTuple*(T: typedesc): bool {.magic: "TypeTrait".} =
  ## Returns true for named tuples, false for any other type.
  runnableExamples:
    doAssert not isNamedTuple(int)
    doAssert not isNamedTuple((string, int))
    doAssert isNamedTuple(tuple[name: string, age: int])

template pointerBase*[T](_: typedesc[ptr T | ref T]): typedesc =
  ## Returns `T` for `ref T | ptr T`.
  runnableExamples:
    assert (ref int).pointerBase is int
    type A = ptr seq[float]
    assert A.pointerBase is seq[float]
    assert (ref A).pointerBase is A # not seq[float]
    assert (var s = "abc"; s[0].addr).typeof.pointerBase is char
  T

proc distinctBase*(T: typedesc, recursive: static bool = true): typedesc {.magic: "TypeTrait".} =
  ## Returns the base type for distinct types, or the type itself otherwise.
  ## If `recursive` is false, only the immediate distinct base will be returned.
  ##
  ## **See also:**
  ## * `distinctBase template <#distinctBase.t,T,static[bool]>`_
  runnableExamples:
    type MyInt = distinct int
    type MyOtherInt = distinct MyInt
    doAssert distinctBase(MyInt) is int
    doAssert distinctBase(MyOtherInt) is int
    doAssert distinctBase(MyOtherInt, false) is MyInt
    doAssert distinctBase(int) is int

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

  proc tupleLen*(T: typedesc[tuple]): int {.magic: "TypeTrait".} =
    ## Returns the number of elements of the tuple type `T`.
    ##
    ## **See also:**
    ## * `tupleLen template <#tupleLen.t>`_
    runnableExamples:
      doAssert tupleLen((int, int, float, string)) == 4
      doAssert tupleLen(tuple[name: string, age: int]) == 2

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

proc rangeBase*(t: typedesc): typedesc {.magic: "TypeTrait".} =
  ## Returns the base type of the ``range`` type `t`. Only a single level is
  ## skipped, that is, if the range type's base type is also a range type,
  ## the base type is not skipped.
  runnableExamples:
    type
      Range = range[1..3]
      Nested = range[Range(1)..Range(3)]

    doAssert rangeBase(Range) is int
    doAssert rangeBase(Range) isnot range
    doAssert rangeBase(Nested) is Range

proc isCyclical*(t: typedesc): bool {.magic: "TypeTrait".} =
  ## Returns whether the type `t` is *potentially* able to be part of a
  ## reference cycle when used as the type of a managed heap location.
  runnableExamples:
    type
      NoCycle = object
        x: seq[NoCycle]
      NoCycle2 {.acyclic.} = ref object
        x: NoCycle2
      Cycle = ref object
        x: Cycle

    doAssert not isCyclical(NoCycle)
    doAssert not isCyclical(NoCycle2)
    doAssert isCyclical(Cycle)

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
