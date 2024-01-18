
discard """
  description: '''
    Describe the evaluation behaviour for `lent T`-returning procedures
  '''
  knownIssue.vm: '''
    `lent T` type where T is a type that fits into a register aren't properly
    supported
  '''
"""

# a special case is made for the first parameter of a `lent T` returning
# procedure. If the *first* parameter is not of `var`, `ptr`, `ref`, or
# `openArray` type, only its sub-expressions are evaluated

proc test1(a, b, v: int): lent int =
  result = a
  doAssert a == v
  doAssert b == v

var x = 0
discard test1(x, (inc x; x), 1)
# the procedure recieves the location of `x`, not its value

# the aforementioned types are excluded from this special case
proc test2(a, b: ptr int, v1, v2: int): lent int =
  result = a[]
  doAssert a[] == v1
  doAssert b[] == v2

var
  y = 2
  z = 3
  p = addr y
discard test2(p, (p = addr z; p), 2, 3)
# the procedure receives the value of `p`, not its location