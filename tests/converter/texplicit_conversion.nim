discard """
  output: "234"
"""

# bug #4432

import strutils

converter toInt(s: string): int =
  result = parseInt(s)

let x = (int)"234"
echo x

block: # test of nkConv
  # int(a) is a l-value so should emit an nkConv for transf.
  # test will show regressions on code generators
  proc foo(o: var int) =
    assert o == 0
  var a = 0
  foo(int(a))
