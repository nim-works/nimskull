discard """
  description: "Regression tests for the VM code-generator"
  targets: vm
"""

import std/parseutils

block:
  # the ``parseBiggestFloat`` magic had an evaluation-order issue where the
  # second parameter was evaluated first
  var
    x = 0
    o: BiggestFloat
  let r = parseBiggestFloat((x = 1; "1.0"),
                            (doAssert x == 1; x = 2; o),
                            (doAssert x == 2; 0))

  doAssert r == 3
  doAssert o == 1.0