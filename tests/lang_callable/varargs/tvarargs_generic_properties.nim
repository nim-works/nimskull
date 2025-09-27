discard """
  description: "Tests for generic varargs properties"
"""


block generics_properties:
  block generic_varargs_infer_the_type_T:
    proc foo[T](v: varargs[T]): int =
      doAssert typeof(v[0]) is T
      v.len

    doAssert foo(1, 2, 3) == 3