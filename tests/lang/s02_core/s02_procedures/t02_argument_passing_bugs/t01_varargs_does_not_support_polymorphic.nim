discard """
description: '''
Passing derived types via varargs raises an "invalid object assignment" exception
instead of passing values directly.

`varargs` is defined to behave the same way as procedure with multiple arguments,
which means derived object should correctly pass a slice/ptr to the procedure.

When `varargs` procedure is called, temporary array with values is constructed, before
being passed to the called procedure. If derived object is passed, correct `.Sup` must
be used to achieve desired behavior.
'''
knownIssue: "varargs cannot pass polymorphic derived types"
"""

var invalidAssigns: seq[string]

block regular_types:
  ## Issue is present for both regular and generic code
  type
    Base = object of RootObj
      fbase: int

    Derived1 = object of Base
      fderived: int

    Derived2 = object of Base
      fderived: int

  block:
    proc test(args: varargs[Base]) = discard

    try:
      test(Base())

    except ObjectAssignmentDefect:
      invalidAssigns.add "Base()"

    try:
      test(Base(), Derived1())

    except ObjectAssignmentDefect:
      invalidAssigns.add "Base(), Derived1()"

    try:
      test(Derived1(), Derived2())

    except ObjectAssignmentDefect:
      invalidAssigns.add "Derived1(), Derived2()"


block:
  type
    Base[T] = object of RootObj
      value: T
    Derived1[T] = object of Base[T]
    Derived2[T] = object of Base[T]


  proc implRegular[T](a, b, c: Base[T]): string =
    result.add $a.value
    result.add $b.value
    result.add $c.value

  proc implVarargs[T](x: varargs[Base[T]]): string =
    discard
    # result = ""
    # for c in x:
    #   result.add $c.value

  doAssert implRegular(
    Derived2[int](value: 2),
    Derived1[int](value: 4),
    Base[int](value: 3)
  ) == "243", "Passing subtypes using regular arguments works correctly"

  try:
    doAssert implVarargs(
      Derived2[int](value: 2),
      Derived1[int](value: 4),
      Base[int](value: 3)
    ) == "243", "Passing subtypes via varargs must work the same way as mutliple arguments"

  except ObjectAssignmentdefect:
    invalidAssigns.add "Derived1, Derived, Base"


doAssert invalidAssigns.len == 0, "Failed object assignment for " & $invalidAssigns.len &
  " cases - " & $invalidAssigns
