discard """
  reject: true
"""

type Imported {.importc.} = object

proc p(y, a, b, c, x: sink Imported) {.tailcall.} =
  discard
