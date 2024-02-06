discard """
  description: "Ensure that overflow checks for `abs` work"
  targets: "c js vm"
  matrix: "--overflowChecks:on"
  exitcode: 1
  outputsub: "over- or underflow"
  knownIssue.js: "64-bit signed integers aren't fully supported"
"""

{.push overflowChecks: off.}

proc test(a: int64) =
  # the overflow behaviour of the ``abs`` call is not affected by the current
  # overflow-check state, so the below fails with an overflow defect
  discard abs(a)

# negating the lowest possible 32-bit integer would overflow
test(low(int64))

{.pop.}