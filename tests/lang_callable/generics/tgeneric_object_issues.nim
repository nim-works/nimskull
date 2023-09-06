discard """
  description: "Regression test for generic objects"
"""

template reject(x: untyped) =
  static:
    doAssert(not compiles(x))

block inherit_from_generic_object_with_generic_param_base:
  # inheriting from a generic object type that has a generic parameter as
  # the base must work
  type
    Base[T] = object of T
      a: int
    Sub[T] = object of Base[T]
      b: string

  var x = Sub[RootObj](a: 1, b: "")

block delay_field_name_checks_until_instantiation:
  # the checks for whether field names are duplicated have to be delayed
  # until instantiation
  type
    Base[T] = object of RootObj
      when T is int:
        a: T
      else:
        b: T

    Sub[T] = object of Base[T]
      b: float

  var x = Sub[int](a: 1, b: 2.0)
  # if the object is not instantiated with an ``int`` type argument, the
  # second branch is chosen and `b` thus is redefined
  reject: Sub[float]()

block requires_init_for_phantom_type:
  # whether the object requires initialization wasn't computed for
  # phantom object types
  type NoDefault {.requiresInit.} = object

  type Phantom[T] = object
    x: NoDefault # has no default value

  reject:
    var v: Phantom[int]

block wrong_field_positions_for_indirect_instance:
  # the positions for fields weren't set properly for indirectly instantiated
  # generic object types inheriting from generic types, which caused internal
  # run-time crashes with the VM target
  type
    Base[T] = object of RootObj
      when T is int:
        a, b, c: T
      else:
        a: int

    Sub[T] = object of Base[T]
      field: string

  proc test[T](x: T): Sub[T] =
    result.field = x # `field` had a position value of '4'

  discard test("str")