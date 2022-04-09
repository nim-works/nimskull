discard """
  output: ''''''
  matrix: "--gc:arc"
  targets: "native"
"""

# bug #13519

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
