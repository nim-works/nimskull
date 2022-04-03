discard """
  targets: "c cpp"
  matrix: "--gc:boehm"
  output: '''meep'''
  disabled: "windows"
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
