discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:tfor --expandArc:texit --hint:Performance:off $file'''
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
      var i_1_cursor = i
      if ==(i_1_cursor, 2):
        return
      add(a,
        var :aux_9 = default()
        =copy(:aux_9, x)
        :aux_9)
      inc(i, 1)
  block :label_0:
    if cond:
      add(a,
        var :aux_10 = x
        wasMoved(x)
        :aux_10)
      break :label_0
    add(b,
      var :aux_11 = x
      wasMoved(x)
      :aux_11)
finally:
  =destroy(x)
  =destroy_1(b)
  =destroy_1(a)
-- end of expandArc ------------------------
--expandArc: texit
var str
var x
try:
  x = $(cond)
  if cond:
    return
  str = $(cond)
  if not(cond):
    result = str
    wasMoved(str)
    return
finally:
  =destroy(x)
  =destroy(str)
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

proc texit(cond: bool): string =
  var str: string
  let x = $cond # starts initialized and requires destruction

  if cond:
    return # make sure `x` escapes

  str = $cond # start `str`'s lifetime

  if not cond:
    result = str # `str` can be moved (str's lifetime ends)
    return # unstructured exit
  # there are no unstructured exits of `str`'s scope where `str` is alive

discard texit(false)