discard """
  target: "c !js !vm"
  matrix: "--deepcopy:on"
  output: "meep"
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
