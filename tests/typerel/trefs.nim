discard """
  targets: native
  errormsg: "type mismatch"
  file: "trefs.nim"
  line: 21
"""
# test for ref types (including refs to procs)

type
  TProc = proc (a, b: int): int {.stdcall.}

proc foo(c, d: int): int {.stdcall.} =
  return 0

proc wrongfoo(c, e: int): int {.inline.} =
  return 0

var p: TProc
p = foo
echo "success!"
p = wrongfoo  #ERROR_MSG type mismatch
