discard """
  description: '''
    A generic procedure expression with an `auto` return type can match against
    a procedure type with no return type
  '''
"""

proc generic[T](x: T): auto =
  discard

proc test(x: proc(x: int)) =
  x(1)

# the return type is inferred as void (read, none/empty)
test generic
