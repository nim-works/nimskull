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
  # the checks for whether field names are duplicated names are delayed until
  # instantiation
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