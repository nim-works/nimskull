discard """
  output: '''("string here", 80)'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:sio --hint:Performance:off $file'''
  nimout: '''--expandArc: main

var :aux_2
try:
  var x_cursor = ("hi", 5)
  block label:
    if cond:
      x_cursor = [type node](("different", 54))
      break label
    x_cursor = [type node](("string here", 80))
  echo([
    var :aux_4 = $(x_cursor)
    :aux_2 = :aux_4
    :aux_2])
finally:
  =destroy(:aux_2)
-- end of expandArc ------------------------
--expandArc: sio

block label:
  var filename_cursor = "debug.txt"
  var f = open(filename_cursor, 0, 8000)
  try:
    var res
    try:
      res = newStringOfCap(80)
      block label_1:
        while true:
          if not(readLine(f, res)):
            break
          block label_2:
            var x_cursor = res
            echo([x_cursor])
    finally:
      =destroy(res)
  finally:
    close(f)
-- end of expandArc ------------------------'''
"""

proc main(cond: bool) =
  var x = ("hi", 5) # goal: computed as cursor

  x = if cond:
        ("different", 54)
      else:
        ("string here", 80)

  echo x

main(false)

proc sio =
  for x in lines("debug.txt"):
    echo x

if false:
  sio()
