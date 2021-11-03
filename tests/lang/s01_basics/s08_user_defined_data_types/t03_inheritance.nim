discard """
description: '''
It is possible for objects to inherit fields from another. Each
object has at most one parent type.
'''
"""

block derive_from_root_obj:
  type
    Base = object of RootObj
      field1: int
      field2: float

    Derived = object of Base
      field3: int

  ## `Derived` type has all newly declared fields as well as all types
  ## declared in parent objects.
  discard Derived(field3: 3, field2: 2, field1: 1)

  ## Omitting fields would result in initializing them to default values
  let it = Derived(field1: 1)

  doAssert it.field1 == 1
  doAssert it.field2 == 0
  doAssert it.field3 == 0


block derive_from_ref_root_obj:
  ## While it is possible to derive regular object, usually inheritance is
  ## used with `ref object` types.
  type
    Base = ref object of RootObj
      f1: int

    Derived = ref object of Base
      f2: int

  ## Using ref object allows to store values of derived type in variable
  ## of base type
  let base: Base = Derived(f2: 12)

  ## Or storing multuple derived objects in the same sequence
  let baseSeq: seq[Base] = @[Base(), Derived()]

  ## To check whether based object is a subtype you can use built-in `of`
  ## operator
  doAssert baseSeq[1] of Derived
  doAssert not(baseSeq[0] of Derived)

  ## To convert base type back to derived you can use object conversion
  doAssert Derived(base).f2 == 12

  ## If expressions cannot be converted to the derived type it will
  ## result in `ObjectConversionDefect` begin raised
  try:
    discard Derived(Base())
    doAssert false, "Had to raise defect"

  except ObjectConversionDefect as def:
    discard
