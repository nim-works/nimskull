discard """
  description: "Tests object variant branch changing"
  action: compile
"""

type A = object
  s: seq[string]
  case kind: range[0..2]
  of 0..1:
    str: string
  of 2:
    i: int

# XXX: not VM specific

static:
  var a: A
  a.s = @["a"]
  a.kind = 2 # change
  a.i = 1
  doAssert a.s[0] == "a"

  a.kind = 0 # change
  a.str = "str"
  doAssert a.s[0] == "a"

  a.kind = 1 # active branch is the same
  doAssert a.s[0] == "a"
  doAssert a.str == "str"

  a.kind = 2 # change
  a.i = 2
  doAssert a.s[0] == "a"