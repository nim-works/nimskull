discard """
description: '''
A `varargs` parameter accepts subtypes, both `ref` and non-`ref` object
types, although it's not clear whether the latter should continue to work
'''
"""

block original:
  type
    Base = ref object of RootObj
    Derived1 = ref object of Base
    Derived2 = ref object of Base

  proc impl(oa: varargs[Base]) = discard

  impl(Base())
  impl(Base(), Derived1())
  impl(Derived1())
  impl(Derived1(), Derived2())

block with_ref_object:
  type
    Base = ref object of RootObj
      fbase: int

    Derived1 = ref object of Base
      fderived: int

    Derived2 = ref object of Base
      fderived: int

  proc impl(args: varargs[Base]) = discard
  impl(Base())
  impl(Base(), Derived1())
  impl(Derived1(), Derived2())

block with_object:
  type
    Base = object of RootObj
      fbase: int

    Derived1 = object of Base
      fderived: int

    Derived2 = object of Base
      fderived: int

  proc impl(args: varargs[Base]) = discard
  impl(Base())
  impl(Base(), Derived1())
  impl(Derived1(), Derived2())