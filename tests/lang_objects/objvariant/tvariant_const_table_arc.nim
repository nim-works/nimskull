discard """
  matrix: "--gc:arc"
  output: "abc: @[(kind: A, x: 0)]"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/14581
      SIGSEGV when using object variant with gc:arc and const Table
    . This works without arc. This also works, when `const t`
      is changed to `var t`
  '''
"""

import std/tables

type E = enum
  A, B

type O = object
  case kind: E
  of A:
    x: int
  of B:
    y: int 

proc someTable(): Table[string, seq[O]] =
  result = initTable[string, seq[O]]()
  result["abc"] = @[O(kind: A)]

const t = someTable()

for k, v in t:
  echo k, ": ", v

