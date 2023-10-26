discard """
"""

import std/strformat

echo fmt"{1}f"

#[
# attempting at creating a minimal failure case
proc main(n: SomeNumber) =
  when n is SomeUnsignedInt:
    var v = n.uint64
    let negative = false
  else:
    let n = n.int64
    let negative = n < 0
    var v =
      if negative:
        # `uint64(-n)`, but accounts for `n == low(int64)`
        uint64(not n) + 1
      else:
        uint64(n)
  
  var w = typeof(v)(10)
  
  doAssert v == w

main(10)
]#