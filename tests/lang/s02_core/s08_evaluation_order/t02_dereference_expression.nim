discard """
  description: "Describe the evaluation behaviour of dereference expressions"
"""

# when evaluating a dereference expression, the dereference target is fully
# evaluated first

var
  a = 1
  b = 2
  p = addr a

# the expression has no effects
doAssert p[] == 1

# the dereference observes the changed pointer value
doAssert (p = addr b; p)[] == 2

var
  c  = [2, 3]
  d  = [4, 5]
  p2 = addr c

# evaluation of outermore expressions is not observable by the dereference
doAssert p2[][(p2 = addr d; 1)] == 3