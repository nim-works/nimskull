discard """
  description: '''
    Pass-by-reference parameters of `.musttail` cannot take arguments
    pointing to local stack frame data
  '''
"""

type Large = object
  # passed by reference
  pad: array[255, int]
  val: int

proc testm(x: Large): int {.musttail.} =
  x.val

# arguments can be parameters:
proc test(x: Large): int =
  testm(x)

doAssert test(Large(val: 1)) == 1

# arguments can be projections of parameters:
proc test_field_projection(a: (Large, int)): int =
  testm(a[0])

proc test_array_projection(a: array[2, Large]): int =
  testm(a[0])

proc test_deref_projection(a: ref Large): int =
  testm(a[])

proc test_var_param(a: var Large): int =
  testm(a)

doAssert test_field_projection((Large(val: 1), 0)) == 1
doAssert test_array_projection([Large(val: 1), Large(val: 2)]) == 1
doAssert test_deref_projection((ref Large)(val: 1)) == 1
var global = Large(val: 1)
doAssert test_var_param(global) == 1

# arguments can be globals (or projections of globals):
proc test_global(): int =
  testm(global)

doAssert test_global() == 1
