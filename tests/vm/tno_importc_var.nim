discard """
  action: reject
  description: "C FFI doesn't work at compile time"
  errormsg: "cannot 'importc' variable/proc at compile time: x"
  line: 10
"""

static:
  var x {.importc: "cx".}: cint # cname doesn't matter here
  var y = x