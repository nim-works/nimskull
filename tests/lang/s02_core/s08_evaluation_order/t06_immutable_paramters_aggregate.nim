discard """
  description: '''
    Describe the evaluation behaviour of arguments to immutable parameters
    that are of large aggregate type.
  '''
"""

type Large = object
  ## A large object.
  val: int
  padding: array[15, int]

proc test(a, b, v1, v2: Large) =
  doAssert a == v1
  doAssert b == v2

var x = Large(val: 1)
# the first argument is evaluated first
test(x, (inc x.val; x), Large(val: 1), Large(val: 2))
# the second argument observes all mutation effects of the first one
test((inc x.val; x), x, Large(val: 3), Large(val: 3))

# indirect mutations are also observed
proc get(): Large =
  inc x.val
  result = x

x.val = 1
test(x, get(), Large(val: 1), Large(val: 2))
test(get(), x, Large(val: 3), Large(val: 3))