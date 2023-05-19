discard """
  description: "Tests for self assignments of types with lifetime hooks"
  targets: "c js vm"
  matrix: "--cursorInference:off"
"""

import mhelper

test self_assignment_with_side_effect:
  # test that for a statically detectable self assignment where both the source
  # and destination expressions have side-effects, the assignment is
  # eliminated, but the expressions still evalauted for their side-effects

  proc prc() =
    var x = initResource()
    var i = 0
    # the compiler must detect that the below is a self assignment
    (inc i; x) = (inc i; x)

    doAssert i == 2

  prc()

  doAssert numCopies == 0
  doAssert numSinks == 0
  doAssert numDestroy == 1
