discard """
  description: '''
    Ensure that templates with `var` and `lent` return types behave the same
    as normal procedures would
  '''
"""

# XXX: they currently don't. Refer to the assertion with the
#      "now works" string

block scalar_type:
  # test with a scalar type
  template borrowVar(x: untyped): var int = x
  template borrowLent(x: untyped): lent int = x

  var v = 0
  borrowVar(v) = 1 # test that an assignment works
  doAssert borrowVar(v) == 1 # test that a read works
  doAssert compiles((borrowLent(v) = 2)), "now works"
  doAssert borrowLent(v) == 1

block tuple_type:
  # test with a tuple type
  type Tuple = tuple[x: int]

  template borrowVar(x: untyped): var Tuple = x
  template borrowLent(x: untyped): lent Tuple = x

  # note: `v` being an unnamed tuple is deliberate, as it is
  # intended to ensure that the implicit conversion in the template
  # doesn't cause problems
  var v: Tuple = (0,)
  # test full assignment and read access:
  borrowVar(v) = (x: 1)
  doAssert borrowVar(v) == (x: 1)
  # test with field write and read:
  borrowVar(v).x = 2
  doAssert borrowVar(v).x == 2

  # test that the lent-returning templates cannot be used for
  # mutations:
  doAssert compiles((borrowLent(v) = (x: 1))), "now works"
  doAssert compiles((borrowLent(v).x = 1)), "now works"

  # test that read-only access is possible:
  doAssert borrowLent(v) == (x: 2)
  doAssert borrowLent(v).x == 2
