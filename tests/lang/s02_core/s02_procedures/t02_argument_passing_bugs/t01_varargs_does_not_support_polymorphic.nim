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
"""

var invalidAssings: seq[string]

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
      invalidAssings.add "Base()"

    try:
      test(Base(), Derived1())
    except ObjectAssignmentDefect:
      invalidAssings.add "Base(), Derived1()"

    try:
      test(Derived1(), Derived2())
    except ObjectAssignmentDefect:
      invalidAssings.add "Derived1(), Derived2()"
