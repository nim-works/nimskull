discard """
  errormsg: "not all cases are covered; missing: {C}"
  line: 23
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

var t1 = case x:
  of A: "a"
  of B: "b"
