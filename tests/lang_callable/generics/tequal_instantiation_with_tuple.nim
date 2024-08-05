discard """
  description: '''
    Ensure that two instantiations of a cyclic generic object type are
    considered equal at every level (type system, backend, run-time) when
    one of them is instantiated with a named tuple and the other with an
    unnamed one
  '''
"""

type Recursive[T] = object
  # the same problem occurred when a ``ref`` type is used
  self: ptr Recursive[T]
  val: T

# it's important that both tuples are comprised of the exact same types
var x: Recursive[(int, int)]
var y: Recursive[tuple[a, b: int]]
x = y
