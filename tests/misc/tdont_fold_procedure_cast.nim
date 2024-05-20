discard """
  description: '''
    Regression test for an issue where a cast to procedural type was folded
    away when the operand was a constant expression
  '''
  targets: c
  matrix: "--expandArc:test"
  nimout: '''
--expandArc: test
scope:
  def_cursor _2: proc (x: float){.nimcall.} = cast other
  def p: proc (x: float){.nimcall.} = copy _2
  def_cursor _3: proc (x: int){.nimcall.} = cast p
  _3(arg 1) -> [Resume]
-- end of expandArc ------------------------
  '''
  output: "1"
"""

proc other(x: int) {.nimcall.} =
  echo x

proc test() =
  # cast to an incompatible procedure type. This must not result in an
  # error
  var p = cast[proc(x: float) {.nimcall.}](other)
  # now cast to the back to the correct type and invoke
  (cast[proc(x: int){.nimcall.}](p))(1)

test()