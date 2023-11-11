discard """
  description: "Regression test to ensure named param errors are reported"
  errormsg: "type mismatch: got <x: int literal(1), y: seq[empty]>"
  line: 9
"""

proc f(y: seq[string]) = return

f(x=1, y = @[])