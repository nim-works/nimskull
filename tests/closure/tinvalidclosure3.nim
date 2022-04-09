discard """
  target: "native"
  errormsg: "illegal capture 'x'"
  line: 10
"""

proc outer(arg: string) =
  var x = 0
  proc inner {.inline.} =
    echo "inner", x
  inner()

outer("abc")
