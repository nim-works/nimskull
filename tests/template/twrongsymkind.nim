discard """
  targets: native
  errormsg: "cannot use symbol of kind 'var' as a 'param'"
  line: 21
"""

# bug #3158

type
  MyData = object
      x: int

template newDataWindow(data: ref MyData): untyped =
    proc testProc(data: ref MyData) =
        echo "Hello, ", data.x
    testProc(data)

var d: ref MyData
new(d)
d.x = 10
newDataWindow(d)
