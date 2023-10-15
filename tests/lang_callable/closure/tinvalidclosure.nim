discard """
  errormsg: "type mismatch: got <proc [T](x: int){.nimcall, gcsafe, locks: 0.}>"
  line: 12
"""

proc ugh[T](x: T) {.nimcall.} =
  echo "ugha"


proc takeCdecl(p: proc (x: int) {.cdecl.}) = discard

takeCDecl(ugh[int])
