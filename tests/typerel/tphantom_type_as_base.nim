discard """
  targets: "c js vm"
  description: '''
    Tests for the type-relation analysis between object types using
    inheritance with phantom types
  '''
"""

block with_object:
  type
    Base[T] = object of RootObj
    Sub     = object of Base[int]

  proc p[T](x: Base[T]): int = 1
  # inferring the generic parameter works:
  doAssert p(Sub()) == 1
  # generic specialization works too:
  doAssert p[int](Sub()) == 1

  proc p(x: Base[int]): int = 2
  # the non-generic overload matches better:
  doAssert p(Sub()) == 2

  proc p(x: Base[string]): int = 0 # unrelated overload; must not be chosen
  doAssert p(Sub()) == 2

  proc p(x: Sub): int = 3
  # the more precise overload matches:
  doAssert p(Sub()) == 3

block with_ref_object:
  type
    Base[T] = ref object of RootObj
    Sub     = ref object of Base[int]

  proc p[T](x: Base[T]): int = 1
  # inferring the generic parameter works:
  doAssert p(Sub()) == 1
  # generic specialization works too:
  doAssert p[int](Sub()) == 1

  proc p(x: Base[int]): int = 2
  # the non-generic overload matches better:
  doAssert p(Sub()) == 2

  proc p(x: Base[string]): int = 0 # unrelated overload; must not be chosen
  doAssert p(Sub()) == 2

  proc p(x: Sub): int = 3
  # the more precise overload matches:
  doAssert p(Sub()) == 3

block with_ptr_object:
  type
    Base[T] = ptr object of RootObj
    Sub     = object of Base[int]

  var obj = Sub()

  proc p[T](x: Base[T]): int = 1
  # inferring the generic parameter works:
  doAssert p(addr obj) == 1
  # generic specialization does too:
  doAssert p[int](addr obj) == 1

  proc p(x: Base[int]): int = 2
  # the non-generic overload matches better:
  doAssert p(addr obj) == 2

  proc p(x: Base[string]): int = 0 # unrelated overload; must not be chosen
  doAssert p(addr obj) == 2

  proc p(x: ptr Sub): int = 3
  # the more precise overload matches:
  doAssert p(addr obj) == 3