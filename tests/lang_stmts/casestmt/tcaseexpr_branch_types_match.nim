discard """
  errormsg: "type mismatch: got <string> but expected 'int literal(10)'"
  line: 25
"""





# line 10
type
  E = enum A, B, C

proc foo(x: int): auto =
  return case x
    of 1..9: "digit"
    else: "number"

var r = foo(10)

var x = C

var t2 = case x:
  of A: 10
  of B, C: "23"
