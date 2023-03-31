discard """
  target: c
  matrix: "--gc:boehm"
  disabled: "windows"
  output: '''meep'''
"""

proc callit(it: proc ()) =
  it()

proc main =
  var outer = "meep"
  proc x =
    echo outer
  var y: proc()
  deepCopy(y, x)
  callit(y)

main()
