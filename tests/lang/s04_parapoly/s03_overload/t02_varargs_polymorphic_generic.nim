discard """
  description: '''
    `varargs` is defined to behave the same way as routine with multiple
    arguments, which means derived objects should correctly pass a slice/ptr to
    the routine.
    
    When a `varargs` routine is called, temporary array with values is
    constructed, before being passed to the called procedure. If derived object
    is passed, correct `.Sup` must be used to achieve desired behavior.
  '''
  knownIssue: '''
    passing derived generic types via varargs raises an "invalid object
    assignment" exception.
  '''
"""

var invalidAssings: seq[string]

block:
  type
    Base[T] = object of RootObj
      value: T
    Derived1[T] = object of Base[T]
    Derived2[T] = object of Base[T]


  proc implRegular[T](a, b, c: Base[T]): string =
    result.add $a.value
    result.add $b.value
    result.add $c.value

  proc implVarargs[T](x: varargs[Base[T]]): string =
    discard
    # result = ""
    # for c in x:
    #   result.add $c.value

  doAssert implRegular(
    Derived2[int](value: 2),
    Derived1[int](value: 4),
    Base[int](value: 3)
  ) == "243", "Passing subtypes using regular arguments works correctly"

  try:
    doAssert implVarargs(
      Derived2[int](value: 2),
      Derived1[int](value: 4),
      Base[int](value: 3)
    ) == "243", "Passing subtypes via varargs must work the same way as mutliple arguments"
  except ObjectAssignmentdefect:
    invalidAssings.add "Derived1, Derived, Base"

doAssert invalidAssings.len == 0, "Failed object assignment for " &
                                  $invalidAssings.len & " cases - " &
                                  $invalidAssings
