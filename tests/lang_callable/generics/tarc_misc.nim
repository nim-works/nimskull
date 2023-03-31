discard """
  matrix: "--gc:arc"
  output: ''''''
"""

# bug #13519
# Not necessarily just arc, we can just more strict about proc type conversions

var unrelated: seq[proc() {.closure, gcsafe.}]

unrelated.add proc () =
  echo "gcsafe"

import tables, sequtils
let t = newTable[int, proc()]()

type
  MyProc = proc() {.closure.}

var result: seq[MyProc] = @[]
for x in t.values:
  result.add(x)
