discard """
  description: '''
    Tests overload resolution where the overload set comes from an explicit
    instantiation
  '''
"""

block single_overload:
  # only a single routine exists in the overload set
  proc p[T](): T =
    result = 1

  doAssert (p[int])() == 1

block two_overloads:
  # all matching generic routines are instantiated before overload
  # resolution (hence the description "early instantiation")
  var
    inst1 {.compileTime.} = 0
    inst2 {.compileTime.} = 0

  proc p[T](x: T): T = # 1
    static: inc inst1
    result = x

  proc p[T](x: string): T = # 2
    static: inc inst2
    result = 2

  proc p[T, U](x: string, y: U): T = # 3
    {.error: "should not be instantiated".}

  # only the overloads with the single generic parameters match (and are
  # instantiated)
  doAssert (p[int])(1) == 1 # resolves to a call of overload #1
  doAssert (p[int])("") == 2 # resolves to a call of overload #2
  static:
    doAssert inst1 == 1
    doAssert inst2 == 1

# XXX: should work, but doesn't. The second overload is currently treated as
#      a redifinition of the first (because generic parameter are not
#      considered)
when false: #block dont_include_non_matching:
  proc p[T](x: T): T =
    static: inc inst3
    result = x

  proc p[T, U](x: T): T =
    {.error: "should not be instantiated".}

  doAssert (p[int])(1) == 1