discard """
  output: ''''''
"""

# bug: https://github.com/nim-lang/Nim/issues/13519
# TODO: rename to something about proc type strictness

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
