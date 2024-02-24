discard """
description: '''
Passing `var` subtype to the varargs results in C codegen error.

When fixed should be added to the `subtype_match/subtype_varargs` spec section.
'''
knownIssue: "cannot pass varargs of var subtype due to codegen error"
"""

type
  Base = object of RootObj
    fbase: int

  Derived1 = object of Base
    fderived: int

  Derived2 = object of Base
    fderived: int

block:
  proc test(arg0, arg1: var Base) =
    arg0 = Base(fbase: 11)
    arg1 = Base(fbase: 22)

  var der1 = Derived1(fderived: 1)
  var der2 = Derived2(fderived: 2)

  test(der1, der2)

  doAssert der1.fbase == 11, "Modified object slice"
  doAssert der1.fderived == 1, "Extra part of the object is left intact"

  doAssert der2.fbase == 22
  doAssert der2.fderived == 2

block:
  ## Varargs of ptr/ref types work
  proc test(args: varargs[ptr Base]) =
    args[0][] = Base(fbase: 11)
    args[1][] = Base(fbase: 22)

  var der1 = Derived1(fderived: 1)
  var der2 = Derived2(fderived: 2)

  test(addr der1, addr der2)

  doAssert der1.fbase == 11 and der1.fderived == 1
  doAssert der2.fbase == 22 and der2.fderived == 2

## It is also possible to pass mutable subtype instances to
## the varargs, and it would behave identically to regular
## procedure with multiple arguments.
proc test(args: varargs[var Base]) =
  args[0] = Base(fbase: 11)
  args[1] = Base(fbase: 22)

var der1 = Derived1(fderived: 1)
var der2 = Derived2(fderived: 2)

test(der1, der2)