discard """
  description: "Tests for globals that have lifetime hooks attached"
  targets: "c js vm"
  matrix: "--cursorInference:off"
"""

import mhelper except test

numCopies = 0
numDestroy = 0
block move_non_module_scope_global:
  var x = initResource()
  var y = x # move -- this is the last use of `x`

doAssert numCopies == 0
doAssert numDestroy == 1

numCopies = 0
numDestroy = 0
block consider_call_with_side_effects:
  # for this test, it's important for both variables to not be located at
  # module-scope but also not inside a procedure
  var x = initResource()
  var y = x # must copy...

  proc p() =
    mutate(x)

  p() # <-- because `x` is mutated here

doAssert numCopies == 1
doAssert numDestroy == 2
