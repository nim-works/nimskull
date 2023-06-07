discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:tfor --hint:Performance:off $file'''
  nimout: '''--expandArc: main

var a
var b
var x
x = f()
block label:
  if cond:
    add(a, x)
    break label
  add(b, x)
`=destroy`(b)
`=destroy`(a)
-- end of expandArc ------------------------
--expandArc: tfor

var a
var b
var x
try:
  x = f()
  block label:
    var i = 0
    block label_1:
      while true:
        if op(`<`(i, 4)):
          break
        block label_2:
          var :tmp
          var i_1_cursor = i
          if `==`(i_1_cursor, 2):
            return
          add(a):
            :tmp = op()
            `=copy`(:tmp, x)
            :tmp
          inc(i, 1)
  block label_3:
    if cond:
      var :tmp_1
      add(a):
        :tmp_1 = x
        op(x)
        :tmp_1
      break label_3
    var :tmp_2
    add(b):
      :tmp_2 = x
      op(x)
      :tmp_2
finally:
  `=destroy`(x)
  `=destroy_1`(b)
  `=destroy_1`(a)
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
