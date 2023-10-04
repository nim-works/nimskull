discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:tfor --hint:Performance:off $file'''
  nimout: '''--expandArc: main

var a
var b
var x
x = f()
block :label_0:
  if cond:
    add(a, x)
    break :label_0
  add(b, x)
=destroy(b)
=destroy(a)
-- end of expandArc ------------------------
--expandArc: tfor

var a
var b
var x
try:
  x = f()
  var a_1 = 0
  var b_1 = 4
  var i = a_1
  block :label_0:
    while true:
      if not(<(i, b_1)):
        break :label_0
      var :aux_9
      var i_1_cursor = i
      if ==(i_1_cursor, 2):
        return
      add(a,
        :aux_9 = default()
        =copy(:aux_9, x)
        :aux_9)
      inc(i, 1)
  block :label_0:
    if cond:
      var :aux_10
      add(a,
        :aux_10 = x
        wasMoved(x)
        :aux_10)
      break :label_0
    var :aux_11
    add(b,
      :aux_11 = x
      wasMoved(x)
      :aux_11)
finally:
  =destroy(x)
  =destroy_1(b)
  =destroy_1(a)
-- end of expandArc ------------------------'''
"""

proc f(): seq[int] =
  @[1, 2, 3]

proc main(cond: bool) =
  var a, b: seq[seq[int]]
  var x = f()
  if cond:
    a.add x
  else:
    b.add x

# all paths move 'x' so no wasMoved(x); destroy(x) pair should be left in the
# AST.

main(false)


proc tfor(cond: bool) =
  var a, b: seq[seq[int]]

  var x = f()

  for i in 0 ..< 4:
    if i == 2: return
    a.add x

  if cond:
    a.add x
  else:
    b.add x

tfor(false)
