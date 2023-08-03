discard """
  targets: "c js vm"
  errormsg: "cannot access a .compileTime location outside of a compile-time-only context"
  line: 9
"""

var a {.compileTime.} = 1

discard a