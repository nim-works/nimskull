discard """
  errormsg: "cannot add to node kind: nnkInt8Lit"
  line: 7
"""
import std/macros
macro t(x: untyped): untyped =
  x.add(newEmptyNode())
t(38'i8)
