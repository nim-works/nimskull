discard """
  description: '''
    Pass-by-reference parameters of `.tailcall` cannot take arguments
    pointing to local stack frame data
  '''
"""

type Large = object
  # passed by reference
  pad: array[255, int]
  val: int

proc p(x: Large): int {.tailcall.} =
  x.val

# arguments can be parameters:
proc test(x: Large): int {.tailcall.} =
  p(x)

doAssert test(Large(val: 1)) == 1

# arguments can be projections of parameters:
proc test_field_projection(a: (Large, int)): int {.tailcall.} =
  p(a[0])

proc test_array_projection(a: array[2, Large]): int {.tailcall.} =
  p(a[0])

proc test_deref_projection(a: ref Large): int {.tailcall.} =
  p(a[])

proc test_var_param(a: var Large): int {.tailcall.} =
  p(a)

doAssert test_field_projection((Large(val: 1), 0)) == 1
doAssert test_array_projection([Large(val: 1), Large(val: 2)]) == 1
doAssert test_deref_projection((ref Large)(val: 1)) == 1
var global = Large(val: 1)
doAssert test_var_param(global) == 1

# arguments can be projections of pointer dereferences:
proc test_ptr_projection(x: Large): int {.tailcall.} =
  var pt = addr x
  p(pt[])

doAssert test_ptr_projection(Large(val: 1)) == 1

# arguments can be globals (or projections of globals):
proc test_global(): int {.tailcall.} =
  p(global)

doAssert test_global() == 1
