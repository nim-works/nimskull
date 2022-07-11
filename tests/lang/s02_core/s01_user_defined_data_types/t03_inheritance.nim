discard """
description: '''
It is possible for objects to inherit fields from another object. Each
object has at most one parent type. This practice is common in Object
Oriented Programming.
'''
"""

block derive_from_root_obj:
  type
    Base = object of RootObj
      field1: int
      field2: float

    Derived = object of Base
      field3: int

  ## `Derived` type has all new declared fields as well as all fields
  ## declared in parent objects.
  discard Derived(field3: 3, field2: 2, field1: 1)

  ## Omitting fields would result in initializing them to default values
  let it = Derived(field1: 1)

  doAssert it.field1 == 1
  doAssert it.field2 == 0
  doAssert it.field3 == 0


block derive_from_ref_root_obj:
  ## While it is possible to inherit from regular objects, usually inheritance is
  ## used with `ref object` types.
  type
    Base = ref object of RootObj
      f1: int

    Derived = ref object of Base
      f2: int

  ## Using ref object allows storing a reference of derived type in variable
  ## of the base type
  let base: Base = Derived(f2: 12)

  ## Or storing multuple derived objects in the same sequence
  let baseSeq: seq[Base] = @[Base(), Derived()]

  ## To check whether the base object is a subtype, you can use the built-in
  ## `of` operator
  doAssert baseSeq[1] of Derived
  doAssert not(baseSeq[0] of Derived)

  ## To convert base type back to derived, you can use object conversion
  doAssert Derived(base).f2 == 12

  when (not defined(js) and not defined(vm)) or
       defined(tryBrokenSpecification):
    # Issue: the VM doesn't raise a user-catchable error for this conversion
    # Bug: js target allows this conversion to happen parent ref -> child ref
    try:
      discard Derived(Base())
      doAssert false, "Should have raised defect"

    except ObjectConversionDefect as def:
      discard
