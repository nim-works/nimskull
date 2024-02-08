discard """
description: '''
PLACEHOLDER
'''
"""

# TODO: this likely needs to move to a later section and not be a part of basic
#       parametric polymorphism.

block builtin_typeclasses:
  block object_class:
    type Obj = object

    doAssert Obj() is object
    doAssert Obj is object
    doAssert Obj is typedesc
    doAssert Obj() isnot typedesc

  block enum_class:
    type Enum = enum a

    doAssert Enum is enum
    doAssert a is enum

  block distinct_class:
    type Dist = distinct int

    doAssert Dist is distinct
    doAssert Dist(12) is distinct

  block proc_class:
    proc test(): int = discard

    doAssert test is proc

  block tuple_Class:
    doAssert (1, 2) is tuple
    doAssert (1, 2) is (int, int)