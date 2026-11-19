discard """
  description: '''
    Tests for making sure .compileTime procedures are automatically evaluated
    at compile-time
  '''
"""

# if they're not evaluated at compile-time, an error would occur

proc simple(x: int): int {.compileTime.} = x + 1

doAssert simple(1) == 2

proc simple_generic[T](x: T): T {.compileTime.} = x

doAssert simple_generic(3) == 3

proc typedesc_generic(T: typedesc): int {.compileTime.} = 4

# test with type name as argument
doAssert typedesc_generic(int) == 4
# test with generic invocation as argument
doAssert typedesc_generic(seq[int]) == 4
# test with tuple type construction as argument
doAssert typedesc_generic(tuple[a, b: int]) == 4

# test with typedesc parameter from outer procedure
proc wrapper(T: typedesc) =
  doAssert typedesc_generic(T) == 4

wrapper(int)
